#lang racket/base
(provide
  column:const
  column:vector
  column:table
  column:indirect
  column:interval
  column:bytes:nat
  column:port
  column:port-indirect
  column:port-string
  interval->dict:ordered
  dict.empty
  dict:ordered:vector
  dict:hash
  enumerator-project
  enumerator-filter
  enumerator-sort
  enumerator-dedup
  enumerator->dict:ordered:vector-flat
  enumerator->dict:ordered:vector-group
  group-fold->hash
  group-fold
  group-fold-ordered
  merge-key-union
  merge-antijoin
  merge-join
  dict-join-unordered
  dict-join-ordered
  hash-join
  dict-antijoin-unordered
  dict-antijoin-ordered
  hash-antijoin
  dict-key-union-unordered
  dict-key-union-ordered
  dict-subtract-unordered
  dict-subtract-ordered
  domain
  )
(require "codec.rkt" "enumerator.rkt" "misc.rkt" "order.rkt" racket/file racket/pretty racket/vector)

;; TODO: benchmark a design based on streams/iterators for comparison

(define (bisect start end i<)
  (let loop ((start start) (end end))
    (if (<= end start) end
      (let ((i (+ start (quotient (- end start) 2))))
        (if (i< i) (loop (+ 1 i) end) (loop start i))))))

(define (bisect-next start end i<)
  (define i (- start 1))
  (let loop ((offset 1))
    (define next (+ i offset))
    (cond ((and (< next end) (i< next)) (loop (arithmetic-shift offset 1)))
          (else (let loop ((i i) (o offset))
                  (let* ((o (arithmetic-shift o -1)) (next (+ i o)))
                    (cond ((= o 0)                      (+ i 1))
                          ((and (< next end) (i< next)) (loop next o))
                          (else                         (loop i    o)))))))))

(define (bisect-prev start end i>)
  (define i end)
  (let loop ((offset 1))
    (define next (- i offset))
    (cond ((and (>= next start) (i> next)) (loop (arithmetic-shift offset 1)))
          (else (let loop ((i i) (o offset))
                  (let* ((o (arithmetic-shift o -1)) (n (- i o)))
                    (cond ((= o 0)                   i)
                          ((and (>= n start) (i> n)) (loop n o))
                          (else                      (loop i o)))))))))

(define table.empty
  (method-lambda
    ((length)                         0)
    ((subtable start.sub (end.sub 0)) table.empty)
    ((columns  start.col (end.col 0)) table.empty)
    ((copy)                           table.empty)
    ((dedup)                          table.empty)
    ((dedup!)                         (void))
    ((sort)                           table.empty)
    ((sort!)                          (void))))

(define (table columns (start 0) (end (vector-length (vector-ref columns 0))))
  (if (= 0 (vector-length columns))
    table.empty
    (let loop ((start start) (end end))
      (define (self-length)         (- end start))
      (define (self-width)          (vector-length columns))
      (define (self-column col)     (vector-ref    columns           col))
      (define (self-ref  col row)   (vector-ref    (self-column col) row))
      (define (self-set! col row v) (vector-set!   (self-column col) row v))
      (define (self-copy)           (table (vector-map (lambda (v.col) (vector-copy v.col start end))
                                                       columns)))
      (define (self-dedup!)
        (define width (self-width))
        (let dedup ((row.prev start) (row (+ start 1)))
          (if (= row end)
            (when (< (+ row.prev 1) end)
              (set! end (+ row.prev 1)))
            (if (let duplicate? ((col 0))
                  (or (= col width)
                      (let ((v.col (self-column col)))
                        (and (equal? (vector-ref v.col row.prev)
                                     (vector-ref v.col row))
                             (duplicate? (+ col 1))))))
              (dedup row.prev (+ row 1))
              (let ((row.prev (+ row.prev 1)))
                (unless (= row.prev row)
                  (let swap! ((col 0))
                    (when (< col width)
                      (let ((v.col (self-column col)))
                        (vector-set! v.col row.prev (vector-ref v.col row))
                        (swap! (+ col 1))))))
                (dedup row.prev (+ row 1)))))))
      (if (<= end start)
        table.empty
        (method-lambda
          ((width)                                      (self-width))
          ((length)                                     (self-length))
          ((ref  col row)                               (self-ref  col row))
          ((set! col row v)                             (self-set! col row v))
          ((subtable start.sub (end.sub (self-length))) (loop (+ start start.sub) (+ start end.sub)))
          ((columns  start.col (end.col (self-width)))  (table (vector-copy columns start.col end.col) start end))
          ((copy)                                       (self-copy))
          ((dedup)                                      (let ((t (self-copy)))
                                                          (t 'dedup!)
                                                          t))
          ((dedup!)                                     (self-dedup!))
          ;; TODO:
          ;((sort ))
          ;((sort! ))
          )))))

(define (table-width    t)                              (t 'width))
(define (table-length   t)                              (t 'length))
(define (table-ref      t col row)                      (t 'ref      col row))
(define (table-set!     t col row v)                    (t 'set!     col row v))
(define (table-subtable t start (end (table-length t))) (t 'subtable start end))
(define (table-columns  t start (end (table-width  t))) (t 'columns  start end))
(define (table-dedup    t)                              (t 'dedup))
(define (table-dedup!   t)                              (t 'dedup!))
(define (table-sort     t)                              (t 'sort))
(define (table-sort!    t)                              (t 'sort!))

(define ((column:const     c)                      _) c)
(define ((column:vector    rows)                   i) (vector-ref rows i))
(define ((column:table     columns)                i) (map (lambda (col) (col i)) columns))
(define ((column:indirect  column.pos column)      i) (column (column.pos i)))
(define ((column:interval  column.pos interval->x) i) (interval->x (column.pos i) (column.pos (+ i 1))))
(define ((column:bytes:nat bs size)                i) (let ((i (* i size)))
                                                        (integer-bytes->integer bs #f #t i (+ i size))))

(define (column:port                     in type) (let ((size.type (sizeof type (void))))
                                                    (lambda (i)
                                                      (file-position in (* i size.type))
                                                      (decode        in type))))
(define (column:port-indirect column.pos in type)   (lambda (i)
                                                      (file-position in (column.pos i))
                                                      (decode        in type)))
(define (column:port-string   column.pos in)      (column:interval
                                                    column.pos
                                                    (lambda (pos.0 pos.1)
                                                      (file-position in pos.0)
                                                      (bytes->string/utf-8 (read-bytes (- pos.1 pos.0) in)))))

(define ((interval->dict:ordered i->key i->value) start end) (dict:ordered i->key i->value start end))

(define dict.empty
  (method-lambda
    ((count)                   0)
    ((=/= _)                   dict.empty)
    ((==  _)                   dict.empty)
    ((<=  _)                   dict.empty)
    ((<   _)                   dict.empty)
    ((>=  _)                   dict.empty)
    ((>   _)                   dict.empty)
    ((bstr-prefix   _)         dict.empty)
    ((bstr-contains _)         dict.empty)
    ((has-key?      _)         #f)
    ((ref _ k.found k.missing) (k.missing))
    ((enumerator/2)            (lambda _ (void)))
    ((enumerator)              (lambda _ (void)))))

(define (dict:ordered i->key i->value start end)
  (let loop ((start start) (end end))
    (define self
      (if (<= end start)
        dict.empty
        (method-lambda
          ((pop)       (loop (+ start 1) end))
          ((count)     (- end start))
          ((top)       (i->value start))
          ((max)       (i->key   (- end 1)))
          ((min)       (i->key   start))
          ((>= key)    (loop (bisect-next start end (lambda (i) (any<?  (i->key i) key))) end))
          ((>  key)    (loop (bisect-next start end (lambda (i) (any<=? (i->key i) key))) end))
          ((<= key)    (loop start (bisect-prev start end (lambda (i) (any<?  key (i->key i))))))
          ((<  key)    (loop start (bisect-prev start end (lambda (i) (any<=? key (i->key i))))))
          ((== key)    ((self '>= key) '<= key))
          ((has-key? key)              (let ((self (self '>= key)))
                                         (and (< 0 (self 'count))
                                              (equal? (self 'min) key))))
          ((ref key k.found k.missing) (let ((self (self '>= key)))
                                         (if (or (< 0 (self 'count))
                                                 (not (equal? (self 'min) key)))
                                           (k.missing)
                                           (k.found (self 'top)))))
          ((enumerator/2)              (lambda (yield)
                                         (let loop ((i start))
                                           (when (< i end)
                                             (yield (i->key i) (i->value i))
                                             (loop (+ i 1))))))
          ((enumerator)                (lambda (yield)
                                         (let loop ((i start))
                                           (when (< i end)
                                             (yield (i->key i))
                                             (loop (+ i 1)))))))))
    self))

(define (dict:ordered:vector rows (t->key (lambda (t) t)) (start 0) (end (vector-length rows)))
  (define (i->value i) (vector-ref rows i))
  (define (i->key   i) (t->key (i->value i)))
  (dict:ordered i->key i->value start end))

(define (dict:hash k=>t)
  (let loop ((k=>t k=>t))
    (if (= (hash-count k=>t) 0)
      dict.empty
      (method-lambda
        ((count)                     (hash-count k=>t))
        ((=/= key)                   (loop (hash-remove k=>t key)))
        ((==  key)                   (if (hash-has-key? k=>t key)
                                       (loop (hash key (hash-ref k=>t key)))
                                       dict.empty))
        ((has-key? key)              (hash-has-key? k=>t key))
        ((ref key k.found k.missing) (if (hash-has-key? k=>t key)
                                       (k.found (hash-ref k=>t key))
                                       (k.missing)))
        ((enumerator/2)              (hash->enumerator/2 k=>t))
        ((enumerator)                (lambda (yield)
                                       (for ((k (in-hash-keys k=>t)))
                                         (yield k))))))))

(define ((merge-join A B) yield)
  (when (and (< 0 (A 'count))
             (< 0 (B 'count)))
    (let loop ((A   A)
               (k.A (A 'min))
               (B   B)
               (k.B (B 'min)))
      (case (compare-any k.A k.B)
        ((-1) (let ((A (A '>= k.B)))
                (when (< 0 (A 'count))
                  (loop A (A 'min) B k.B))))
        (( 1) (let ((B (B '>= k.A)))
                (when (< 0 (B 'count))
                  (loop A k.A B (B 'min)))))
        (else (let ((t.A (A 'top))
                    (t.B (B 'top))
                    (A   (A 'pop))
                    (B   (B 'pop)))
                (yield k.A t.A t.B)
                (when (and (< 0 (A 'count))
                           (< 0 (B 'count)))
                  (loop A (A 'min) B (B 'min)))))))))

(define ((merge-antijoin A B) yield)
  ((dict-antijoin-ordered (A 'enumerator/2) B) yield))

(define ((merge-key-union A B) yield)
  ((dict-key-union-ordered (A 'enumerator) B) yield))

(define (group-fold->hash en v.0 f)
  (define k=>v (hash))
  (en (lambda (k v) (set! k=>v (hash-update k=>v k
                                            (lambda (v.current) (f v v.current))
                                            v.0))))
  k=>v)

(define ((group-fold en v.0 f) yield)
  ((hash->enumerator/2 (group-fold->hash en v.0 f)) yield))

(define ((group-fold-ordered en v.0 f) yield)
  (let ((first?    #t)
        (k.current #f)
        (v.current v.0))
    (en (lambda (k v)
          (cond (first?               (set! first?    #f)
                                      (set! k.current k)
                                      (set! v.current (f v v.0)))
                ((equal? k k.current) (set! v.current (f v v.current)))
                (else                 (yield k.current v.current)
                                      (set! k.current k)
                                      (set! v.current (f v v.0))))))
    (unless first?
      (yield k.current v.current))))

(define ((enumerator-dedup en) yield)
  (define first?     #t)
  (define t.previous #f)
  (en (lambda (t)
        (cond (first?                      (set! first?     #f)
                                           (set! t.previous t)
                                           (yield t))
              ((not (equal? t t.previous)) (set! t.previous t)
                                           (yield t))))))

(define ((enumerator-project en f) yield)
  (en (lambda args (apply f yield args))))

(define ((enumerator-filter en ?) yield)
  (en (lambda (t) (when (? t) (yield t)))))

(define ((enumerator-sort en <?) yield)
  ((list->enumerator (sort (enumerator->rlist en) <?)) yield))

(define (enumerator->dict:ordered:vector-flat en (t->key (lambda (t) t)))
  (dict:ordered:vector
    (enumerator->vector (enumerator-dedup (enumerator-sort en any<?)))
    t->key))

(define (enumerator->dict:ordered:vector-group en t->key)
  (dict:ordered:vector
    (enumerator->vector
      (enumerator-project
        (group-fold-ordered
          (enumerator-project (enumerator-dedup (enumerator-sort en any<?))
                              (lambda (yield t) (yield (t->key t) t)))
          '() cons)
        (lambda (yield _ ts.reversed) (yield (reverse ts.reversed)))))
    (lambda (ts) (t->key (car ts)))))

(define ((hash-join en en.hash) yield)
  ((dict-join-unordered en (dict:hash (group-fold->hash en.hash '() cons)))
   (lambda (k t ts.hash)
     (for ((t.hash (in-list (reverse ts.hash))))  ; is this reversal necessary?
       (yield k t t.hash)))))

(define ((hash-antijoin en en.hash) yield)
  ((dict-antijoin-unordered en (dict:hash (group-fold->hash en.hash (void) (lambda _ (void)))))
   yield))

(define ((dict-join-unordered en d.index) yield)
  (when (< 0 (d.index 'count))
    (en (lambda (k v) (d.index 'ref k
                               (lambda (v.index) (yield k v v.index))
                               (lambda ()        (void)))))))

(define ((dict-join-ordered en.ordered d.index) yield)
  (when (< 0 (d.index 'count))
    (en.ordered (lambda (k v)
                  (set! d.index (d.index '>= k))
                  (d.index 'ref k
                           (lambda (v.index) (yield k v v.index))
                           (lambda ()        (void)))))))

(define ((dict-antijoin-unordered en d.index) yield)
  (en (if (= 0 (d.index 'count))
        yield
        (lambda (k v) (unless (d.index 'has-key? k)
                        (yield k v))))))

(define ((dict-antijoin-ordered en.ordered d.index) yield)
  (en.ordered (if (= 0 (d.index 'count))
                yield
                (lambda (k v)
                  (set! d.index (d.index '>= k))
                  (unless (d.index 'has-key? k)
                    (yield k v))))))

(define ((dict-subtract-unordered en d.index) yield)
  (en (if (= 0 (d.index 'count))
        yield
        (lambda (k) (unless (d.index 'has-key? k)
                      (yield k))))))

(define ((dict-subtract-ordered en.ordered d.index) yield)
  (en.ordered (if (= 0 (d.index 'count))
                yield
                (lambda (k)
                  (set! d.index (d.index '>= k))
                  (unless (d.index 'has-key? k)
                    (yield k))))))

(define ((hash-key-union en en.hash) yield)
  (define d.index (dict:hash (let ((k=> (hash)))
                               (en.hash (lambda (k) (set! k=> (hash-set k=> k (void)))))
                               k=>)))
  ((dict-key-union-unordered en d.index) yield))

(define ((dict-key-union-unordered en d.index) yield)
  ((dict-subtract-unordered en d.index) yield)
  ((d.index 'enumerator)                yield))

(define ((dict-key-union-ordered en.ordered d.index) yield)
  (en.ordered (if (= 0 (d.index 'count))
                yield
                (lambda (k)
                  (let loop ()
                    (if (= 0 (d.index 'count))
                      (yield k)
                      (let ((k.d (d.index 'min)))
                        (case (compare-any k k.d)
                          ((-1) (yield k))
                          (( 1) (set! d.index (d.index 'pop))
                                (yield k.d)
                                (loop))
                          (else (set! d.index (d.index 'pop))
                                (yield k))))))))))


;; TODO: computing fixed points?

(define (domain path type . pargs)
  (unless (equal? type 'string) (error "unimplemented domain type:" type))
  (define max-byte-count (plist-ref pargs 'max-byte-count (- (expt 2 40) 1)))
  ;; TODO: support these parameters
  ;(define index?         (plist-ref pargs 'index?         #t))
  ;(define unique?        (plist-ref pargs 'unique?        #t))
  ;(define sorted?        (plist-ref pargs 'sorted?        #t))
  (define verbose?       (plist-ref pargs 'verbose?       #t))
  (define fn.value       "value")
  (define fn.pos         "position")
  (define fn.id          "id")
  (define size.pos       (min-bytes max-byte-count))
  (when verbose? (printf "creating path: ~s\n" path))
  (make-directory* path)
  (define out.metadata   (open-output-file (build-path path "metadata.scm")))
  (define out.value      (open-output-file (build-path path fn.value)))
  (define out.pos        (open-output-file (build-path path fn.pos)))
  (define out.id         (open-output-file (build-path path fn.id)))
  (define value=>id      (make-hash))
  (define (write-pos)    (write-bytes (nat->bytes size.pos (file-position out.value)) out.pos))
  (define (insert value) (unless (hash-has-key? value=>id value)
                           (write-pos)
                           (write-bytes (string->bytes/utf-8 value) out.value)
                           (hash-set! value=>id value (hash-count value=>id))))
  (define (close)
    (write-pos)
    (define size.id (min-bytes (hash-count value=>id)))
    (define (sorted-ids) (sort (hash->list value=>id) (lambda (a b) (string<? (car a) (car b)))))
    (define (write-ids)  (for-each (lambda (s&id) (write-bytes (nat->bytes size.id (cdr s&id)) out.id))
                                   (sorted-ids)))
    (when verbose? (printf "~s: sorting ~s ids\n" path (hash-count value=>id)))
    (if   verbose? (time (sorted-ids)) (sorted-ids))
    (when verbose? (printf "~s: writing ids\n" path))
    (if   verbose? (time (write-ids)) (write-ids))
    (define metadata `((type  . domain)
                       (count . ,(hash-count value=>id))
                       (value . ((path     . ,fn.value)
                                 (type     . ,type)
                                 (position . ((path . ,fn.pos)
                                              (type . nat)
                                              (size . ,size.pos)))))
                       (id    . ((path . ,fn.id)
                                 (type . nat)
                                 (size . ,size.id)))))
    (when verbose?
      (printf "~s: writing metadata\n" path)
      (pretty-write metadata))
    (pretty-write metadata out.metadata)
    (close-output-port out.id)
    (close-output-port out.pos)
    (close-output-port out.value)
    (close-output-port out.metadata))
  (method-lambda
    ((close)        (close))
    ((insert value) (insert value))))

(module+ test
  (require racket/pretty)

  (define (test.0 yield.0)
    (define (yield . args)
      (pretty-write `(yielding: . ,args))
      (apply yield.0 args))
    (yield 0 1)
    (yield 0 2)
    (yield 0 3)
    (yield 1 1)
    (yield 1 2)
    (yield 5 2)
    (yield 5 7))

  (define test.1 (enumerator->enumerator/2
                   (vector->enumerator '#((0 . 1)
                                          (0 . 2)
                                          (0 . 3)
                                          (1 . 1)
                                          (1 . 2)
                                          (5 . 2)
                                          (5 . 7)))))

  (displayln 'group-fold.0)
  ((group-fold test.0 0 +) (lambda (k v) (pretty-write (list k v))))

  (displayln 'group-fold-ordered.0)
  ((group-fold-ordered test.0 0 +) (lambda (k v) (pretty-write (list k v))))

  (displayln 'group-fold.1)
  ((group-fold test.1 0 +) (lambda (k v) (pretty-write (list k v))))

  (displayln 'group-fold-ordered.1)
  ((group-fold-ordered test.1 0 +) (lambda (k v) (pretty-write (list k v))))

  (displayln 'hash-join)
  ((hash-join
     (enumerator->enumerator/2 (list->enumerator '((5 . 6) (10 . 17) (8 . 33) (1 . 5) (0 . 7) (18 . 3))))
     (enumerator->enumerator/2 (list->enumerator '((7 . 61) (10 . 20) (18 . 33) (11 . 5) (0 . 77) (8 . 3)))))
   (lambda (k a b) (pretty-write (list k a b))))

  (displayln 'merge-join)
  ((merge-join
     (enumerator->dict:ordered:vector-group
       (list->enumerator '((5 . 6) (10 . 17) (8 . 33) (1 . 5) (0 . 7) (18 . 3)))
       car)
     (enumerator->dict:ordered:vector-group
       (list->enumerator '((7 . 61) (10 . 20) (18 . 33) (11 . 5) (0 . 77) (8 . 3)))
       car))
   (lambda (k a b) (pretty-write (list k a b))))

  (displayln 'hash-key-union)
  ((hash-key-union
     (list->enumerator (map car '((5 . 6) (10 . 17) (8 . 33) (1 . 5) (0 . 7) (18 . 3))))
     (list->enumerator (map car '((7 . 61) (10 . 20) (18 . 33) (11 . 5) (0 . 77) (8 . 3)))))
   pretty-write)

  (displayln 'merge-key-union)
  ((merge-key-union
     (enumerator->dict:ordered:vector-group
       (list->enumerator '((5 . 6) (10 . 17) (8 . 33) (1 . 5) (0 . 7) (18 . 3)))
       car)
     (enumerator->dict:ordered:vector-group
       (list->enumerator '((7 . 61) (10 . 20) (18 . 33) (11 . 5) (0 . 77) (8 . 3)))
       car))
   pretty-write)

  (displayln 'hash-antijoin)
  ((hash-antijoin
     (enumerator->enumerator/2 (list->enumerator '((5 . 6) (10 . 17) (8 . 33) (1 . 5) (0 . 7) (18 . 3))))
     (enumerator->enumerator/2 (list->enumerator '((7 . 61) (10 . 20) (18 . 33) (11 . 5) (0 . 77) (8 . 3)))))
   (lambda (k v) (pretty-write (list k v))))

  (displayln 'merge-antijoin)
  ((merge-antijoin
     (enumerator->dict:ordered:vector-group
       (list->enumerator '((5 . 6) (10 . 17) (8 . 33) (1 . 5) (0 . 7) (18 . 3)))
       car)
     (enumerator->dict:ordered:vector-group
       (list->enumerator '((7 . 61) (10 . 20) (18 . 33) (11 . 5) (0 . 77) (8 . 3)))
       car))
   (lambda (k v) (pretty-write (list k v))))

  (displayln 'table)
  (let ((t (table (vector (vector  0  0  0  0  1  1  1  2  2  3  3  3  3  3  4)
                          (vector 'a 'a 'a 'b 'a 'a 'b 'a 'a 'a 'b 'b 'c 'c 'a)
                          (vector  0  1  1  1  0  1  1  2  2  1  1  1  1  1  7)))))
    (for ((col (in-range (table-width t))))
      (for ((row (in-range (table-length t))))
        (printf "~s " (table-ref t col row)))
      (newline))

    (displayln 'table-dedup!)
    (table-dedup! t)

    (for ((col (in-range (table-width t))))
      (for ((row (in-range (table-length t))))
        (printf "~s " (table-ref t col row)))
      (newline)))
  )
