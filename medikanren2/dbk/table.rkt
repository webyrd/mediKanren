#lang racket/base
(provide materialize-relation! materialization value/syntax table
         (struct-out statistics) statistics-intersect
         vector-table? call/files let/files encoder s-encode s-decode)
(require "codec.rkt" "config.rkt" "dsv.rkt" "method.rkt" "misc.rkt"
         "order.rkt" "stream.rkt"
         racket/file racket/function racket/hash racket/list racket/match
         racket/pretty racket/set racket/vector)

(define (s-encode out type s) (s-each (lambda (v) (encode out type v)) s))
(define (s-decode in type)
  (thunk (let loop () (if (eof-object? (peek-byte in)) '()
                        (cons (decode in type) (thunk (loop)))))))

(define (encoder out type) (method-lambda ((put! v) (encode out type v))
                                          ((close) (flush-output out))))

(define (call/files fins fouts p)
  (let loop ((fins fins) (ins '()))
    (if (null? fins)
      (let loop ((fouts fouts) (outs '()))
        (if (null? fouts)
          (apply p (append (reverse ins) (reverse outs)))
          (call-with-output-file
            (car fouts) (lambda (out) (loop (cdr fouts) (cons out outs))))))
      (call-with-input-file
        (car fins) (lambda (in) (loop (cdr fins) (cons in ins)))))))

(define-syntax-rule (let/files ((in fin) ...) ((out fout) ...) body ...)
  (call/files (list fin ...) (list fout ...)
              (lambda (in ... out ...) body ...)))

(struct statistics (ratio cardinality) #:prefab)
(define (statistics-intersect a b)
  (match-define (statistics r.a c.a) a)
  (match-define (statistics r.b c.b) b)
  (statistics (min r.a r.b) (min c.a c.b)))

;; TODO: can we simplify by virtually separating bisecting tries from random-access tries?
;;       technically each prefix of a trie has either bisecting or random-access behavior
;; virtualized tries can subsume each other when making progress: a kind of exclusive-or trie
;; - exclusive-or (for grouping with subsumption)
;; - random-access/position-key (each column value is a table position)
;; - bisect-key                 (each column value has a unique entry)
;; - bisect-multi               (each column value may have multiple entries)
;(define (trie:xor ))
;; TODO: this one is for monotone dependencies (grouping without subsumption)
;(define (trie:or ))
;; TODO: there could be tabular and columnar varieties of these
;(define (trie:position-key ))
;(define (trie:bisect-key ))
;(define (trie:bisect-multi ))

;; TODO: support multiple sorted columns using tables that share key columns
;;       (wait until column-oriented tables are implemented for simplicity?)

(define (table ixs)
  (and (not (ormap not ixs))
       (let table ((ixs         (filter (lambda (ix) (not (ix 'done?))) ixs))
                   (col=>bounds (foldl (lambda (ix c=>b)
                                         (hash-union c=>b (ix 'bounds)
                                                     ;; TODO: verify no errors during testing.
                                                     ;;       Should be able to replace this with:
                                                     ;;       #:combine (lambda (b.0 b.1) b.0)
                                                     #:combine/key
                                                     (lambda (k b.0 b.1)
                                                       (if (equal? b.0 b.1)
                                                         b.0
                                                         (error "incompatible initial bounds:" k b.0 b.1)))))
                                       (hash) ixs)))
         (method-lambda
           ((done?)      (null? ixs))
           ((bounds)     col=>bounds)
           ((statistics) (apply hash-union (hash)
                                (map (lambda (ix) (ix 'statistics)) ixs)
                                #:combine statistics-intersect))
           ((update cbs)
            (let loop ((c=>b (foldl (lambda (cb c=>b) (hash-set c=>b (car cb) (cdr cb)))
                                    col=>bounds cbs))
                       (ixs.pending ixs)
                       (ixs.updated '()))
              (if (null? ixs.pending)
                (table (reverse ixs.updated) c=>b)
                (let*/and ((ix.new ((car ixs.pending) 'update c=>b))
                           (c=>b   (hash-union c=>b (ix.new 'bounds)
                                               #:combine (lambda (v.0 v.1) v.1))))
                  (cond ((not (ix.new 'done?)) (loop c=>b (cdr ixs.pending) (cons ix.new ixs.updated)))
                        ((not (ix.new 'full?)) (loop c=>b (cdr ixs.pending)              ixs.updated))
                        (else                  (table '() c=>b)))))))))))

(define (tabular-trie vref key-column nonkey-columns types row-count)
  (define (ref mask i)          (vector-ref (vref i) mask))
  (define ((make-i<  mask v) i) (any<?  (ref mask i) v))
  (define ((make-i<= mask v) i) (any<=? (ref mask i) v))
  (define ((make-i>  mask v) i) (any<?  v (ref mask i)))
  (define ((make-i>= mask v) i) (any<=? v (ref mask i)))
  (define (update/pending c=>b.new cols.pending col=>bounds mask start end)
    (define c.next    (car cols.pending))
    (define b.current (hash-ref col=>bounds c.next bounds.any))
    (define b         (bounds-intersect (hash-ref c=>b.new c.next bounds.any)
                                        b.current))
    (define (update/bounds lb lbi? ub ubi?)
      (define start.new (bisect-next start end ((if lbi? make-i< make-i<=) mask lb)))
      (define end.new (if (< start.new end)
                        (bisect-prev start end ((if ubi? make-i> make-i>=) mask ub))
                        start.new))
      (update/trim c=>b.new cols.pending col=>bounds mask start.new end.new))
    (cond ((equal? b b.current) (new cols.pending col=>bounds mask start end))
          (else                 (update/bounds (bounds-lb b)
                                               (bounds-lb-inclusive? b)
                                               (bounds-ub b)
                                               (bounds-ub-inclusive? b)))))
  (define (update/trim c=>b.new cols.pending col=>bounds mask start end)
    (if (null? cols.pending)
      (new '() (hash-set col=>bounds key-column (bounds start #t (- end 1) #t))
           mask start end)
      (and (< start end)
           (let ((c.next (car cols.pending))
                 (lb     (ref mask start))
                 (ub     (ref mask (- end 1))))
             (if (equal? lb ub)
               (update/trim c=>b.new (cdr cols.pending)
                            (hash-set col=>bounds c.next (bounds lb #t lb #t))
                            (+ mask 1) start end)
               (let ((col=>bounds (hash-set (hash-set col=>bounds c.next (bounds lb #t ub #t))
                                            key-column (if (= start (- end 1))
                                                         start
                                                         (bounds start #t (- end 1) #t)))))
                 (update/pending c=>b.new cols.pending col=>bounds mask start end)))))))
  (define (new cols.pending col=>bounds mask start end)
    (method-lambda
      ;; TODO: this will be incorrect if the nonkey-columns do not map to a single key, which
      ;; can happen when the table includes the key-column as an attribute.
      ((done?)  (null? cols.pending))
      ;; TODO: allow full? to be #t when nonkey-columns include all attributes
      ((full?)  (not (not key-column)))
      ((bounds) col=>bounds)
      ((statistics)
       (define ratio (/ (- end start) row-count))
       (make-immutable-hash
         (cons (cons (car cols.pending)
                     (statistics ratio
                                 (let ((v.lb (ref mask start))
                                       (v.ub (ref mask (- end 1))))
                                   (if (equal? v.lb v.ub)
                                     1
                                     (+ 2 (- (bisect-prev start end (make-i>= mask v.ub))
                                             (bisect-next start end (make-i<= mask v.lb))))))))
               (if key-column
                 (list (cons key-column (statistics ratio (- end start))))
                 '()))))
      ((update c=>b)
       (cond (key-column
               (define b.key.0 (hash-ref col=>bounds key-column bounds.any))
               (define b       (hash-ref c=>b        key-column bounds.any))
               (cond ((not (equal? b b.key.0))
                      ;; Assume these bounds will always be numbers in c=>b
                      (let ((lb (let ((lb.0 (bounds-lb b)))
                                  (cond ((not (integer? lb.0))    (ceiling lb.0))
                                        ((bounds-lb-inclusive? b)          lb.0)
                                        (else                           (+ lb.0 1)))))
                            (ub (let ((ub.0 (bounds-ub b)))
                                  (cond ((not (integer? ub.0))    (floor ub.0))
                                        ((bounds-ub-inclusive? b)        ub.0)
                                        (else                         (- ub.0 1))))))
                        (and (<= start lb ub) (< ub end)
                             (update/trim c=>b cols.pending col=>bounds mask lb (+ ub 1)))))
                     (else (update/pending c=>b cols.pending col=>bounds mask start end))))
             (else         (update/pending c=>b cols.pending col=>bounds mask start end))))))
  (update/trim (hash) nonkey-columns (hash) 0 0 row-count))

(define (table-length t key)       (statistics-cardinality (hash-ref (t 'statistics) key)))
(define (table-ref    t key i col) (bounds-lb (hash-ref ((t 'update (hash key (bounds i #t i #t))) 'bounds) col)))

(define (table/port/offsets table.offsets key-col cols types in)
  (define type `#(tuple ,@types))
  (define (ref i)
    (file-position in (table-ref table.offsets #t i 'offset))
    (decode in type))
  (and table.offsets
       (tabular-trie ref key-col cols types (table-length table.offsets #t))))

(define (table/bytes/offsets table.offsets key-col cols types bs)
  (define in (open-input-bytes bs))
  (table/port/offsets table.offsets key-col cols types in))

(define (table/port len key-col cols types in)
  (define type `#(tuple ,@types))
  (define width (sizeof type (void)))
  (define (ref i) (file-position in (* i width)) (decode in type))
  (tabular-trie ref key-col cols types len))

(define (table/bytes key-col cols types bs)
  (define in (open-input-bytes bs))
  (table/port (quotient (bytes-length bs) (sizeof types (void)))
              key-col cols types in))

(define (table/vector key-col cols types v)
  (tabular-trie (lambda (i) (vector-ref v i)) key-col cols types (vector-length v)))

(define (table.old vref key-col cols.all types row-count)
  (let table ((cols  cols.all)
              (types types)
              (key-bound? #f) (bound '()) (mask 0) (start 0) (end row-count))
    (and
      (< start end)
      (let ()
        (define (ref i)          (vector-ref (vref i) mask))
        (define ((make-i<  v) i) (any<?  (ref i) v))
        (define ((make-i<= v) i) (any<=? (ref i) v))
        (define ((make-i>  v) i) (any<?  v (ref i)))
        (define ((make-i>= v) i) (any<=? v (ref i)))
        (define (trim start end) (table cols types #f bound mask start end))
        (define (skip<= v)       (bisect-next start end (make-i<= v)))
        (define (skip<  v)       (bisect-next start end (make-i<  v)))
        (define (skip>= v)       (bisect-prev start end (make-i>= v)))
        (define (skip>  v)       (bisect-prev start end (make-i>  v)))
        (define (lb)             (ref start))
        (define (ub)             (ref (- end 1)))
        (define (key-col? col)
          (cond ((and (not key-bound?) (equal? col key-col)) #t)
                ((and (pair? cols) (equal? col (car cols)))  #f)
                (else (error "invalid fast column:"
                             col cols key-bound? key-col cols.all))))
        (define (key><= v)
          (cond ((integer?  v) (min (max start       (+ v 1)) end))
                ((rational? v) (min (max start (ceiling v  )) end))
                ((null?     v) start)
                (else          end)))
        (define (key>=< v)
          (cond ((integer?  v) (min (max start          v   ) end))
                ((rational? v) (min (max start (ceiling v  )) end))
                ((null?     v) start)
                (else          end)))
        (method-lambda
          ((columns.bound) (if key-bound? (cons key-col bound) bound))
          ((columns.fast)  (if key-bound? (s-take 1 cols)
                             (cons key-col (s-take 1 cols))))
          ((columns)       (cons key-col cols.all))
          ((size-ratio)    (/ (- end start) row-count))
          ((max-count col) (cond ((key-col? col) (- end start))
                                 (else (let ((v.lb (lb)) (v.ub (ub)))
                                         (if (equal? v.lb v.ub) 1
                                           (+ 2 (- (skip>= v.ub)
                                                   (skip<= v.lb))))))))
          ((min col)  (if (key-col? col) start (lb)))
          ((max col)  (if (key-col? col) end   (ub)))
          ((>  col v) (trim (if (key-col? col) (key><= v) (skip<= v)) end))
          ((>= col v) (trim (if (key-col? col) (key>=< v) (skip<  v)) end))
          ((<  col v) (trim start (if (key-col? col) (key>=< v) (skip>= v))))
          ((<= col v) (trim start (if (key-col? col) (key><= v) (skip>  v))))
          ((=  col v) (if (key-col? col)
                        (and (integer? v)
                             (table cols types #t bound mask v
                                    (if (and (<= start v) (< v end))
                                      (+ v 1) v)))
                        (let* ((start.new (skip< v))
                               (end.new   (bisect-next start.new end
                                                       (make-i<= v))))
                          (table (cdr cols) (cdr types) key-bound?
                                 (cons col bound) (+ mask 1)
                                 start.new end.new)))))))))

(define (vector-table? types v)
  (define v< (compare-><? (type->compare types)))
  (define i (- (vector-length v) 1))
  (or (<= i 0) (let loop ((i (- i 1)) (next (vector-ref v i)))
                 (define current (vector-ref v i))
                 (and (v< current next) (or (= i 0) (loop (- i 1) current))))))
(define (vector-table-sort! types v)
  (vector-sort! v (compare-><? (type->compare `#(tuple ,@types)))))
(define (vector-dedup v) (list->vector (s-dedup (vector->list v))))

;; TODO: switch back to file->bytes once Racket IO bug is fixed
(define (file->bytes2 file-name)
  (define size (file-size file-name))
  (define bs (make-bytes size))
  (call-with-input-file
    file-name
    (lambda (in)
      (let loop ((i 0))
        (cond ((= i size) bs)
              (else (define end (+ i (min 1073741824
                                          (- size i))))
                    (read-bytes! bs in i end)
                    (loop end)))))))

(define (file-stats path)
  (and (file-exists? path)
       `((size . ,(file-size path))
         (time . ,(file-or-directory-modify-seconds path)))))

(define (table/metadata retrieval-type directory-path info-alist)
  (define info         (make-immutable-hash info-alist))
  (define path-prefix
    (path->string (build-path directory-path (hash-ref info 'file-prefix))))
  (define fname.value  (value-table-file-name  path-prefix))
  (define fname.offset (offset-table-file-name path-prefix))
  (define offset-type  (hash-ref info 'offset-type))
  (define fstat.value  (file-stats fname.value))
  (define fstat.offset (and offset-type (file-stats fname.offset)))
  (define key-name     (hash-ref info 'key-name))
  (define column-names (hash-ref info 'column-names))
  (define column-types (hash-ref info 'column-types))
  (define len          (hash-ref info 'length))
  (unless (set=? fstat.value (hash-ref info 'value-file))
    (error "value file stats do not match metadata:" fname.value
           'file: fstat.value 'metadata: (hash-ref info 'value-file)))
  (when offset-type
    (unless (set=? fstat.offset (hash-ref info 'offset-file))
      (error "offset file stats do not match metadata:" fname.offset
             'file: fstat.offset 'metadata: (hash-ref info 'offset-file))))
  (case retrieval-type
    ((disk) (define in.value (open-input-file fname.value))
            (if offset-type
              (table/port/offsets
                (table/port len #t '(offset) `(,offset-type)
                            (open-input-file fname.offset))
                key-name column-names column-types in.value)
              (table/port len key-name column-names column-types in.value)))
    ((bytes) (define bs.value (file->bytes2 fname.value))
             (if offset-type
               (table/bytes/offsets
                 (table/bytes #t '(offset) `(,offset-type)
                              (file->bytes2 fname.offset))
                 key-name column-names column-types bs.value)
               (table/bytes key-name column-names column-types bs.value)))
    ((scm) (let/files ((in.value fname.value)) ()
             (table/vector
               key-name column-names column-types
               (list->vector
                 (s-take #f (s-decode in.value `#(tuple ,@column-types)))))))
    (else (error "unknown retrieval type:" retrieval-type))))

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

(define (value-table-file-name  prefix) (string-append prefix ".value.table"))
(define (offset-table-file-name prefix) (string-append prefix ".offset.table"))
(define metadata-file-name              "metadata.scm")

(define (tabulator directory-path file-prefix
                   column-names column-types key-name)
  (define (unique?! as) (unless (= (length (remove-duplicates as)) (length as))
                          (error "duplicates:" as)))
  (unless (= (length column-names) (length column-types))
    (error "mismatching column names and types:" column-names column-types))
  (unique?! column-names)
  (when (member key-name column-names)
    (error "key name must be distinct:" key-name column-names))
  (define row-type column-types)
  (define row<     (compare-><? (type->compare row-type)))
  (define row-size (sizeof row-type (void)))
  (define path-prefix (path->string (build-path directory-path file-prefix)))
  (define value-file-name  (value-table-file-name path-prefix))
  (define offset-file-name (and (not row-size)
                                (offset-table-file-name path-prefix)))
  (define tsorter (sorter #t value-file-name offset-file-name row-type row<))
  (method-lambda
    ((put! x) (tsorter 'put! x))
    ((close) (match-define (cons offset-type item-count) (tsorter 'close))
             `((file-prefix    . ,file-prefix)
               (value-file     . ,(file-stats value-file-name))
               (offset-file    . ,(and offset-file-name
                                       (file-stats offset-file-name)))
               (offset-type    . ,offset-type)
               (length         . ,item-count)
               (column-names   . ,column-names)
               (column-types   . ,column-types)
               (key-name       . ,key-name)))))

(define (sorter dedup? value-file-name offset-file-name? type value<)
  (define fname-sort-value  (string-append value-file-name ".value.sort"))
  (define fname-sort-offset (string-append value-file-name ".offset.sort"))
  (define out-value         (open-output-file value-file-name))
  (define out-offset
    (and offset-file-name?  (open-output-file offset-file-name?)))
  (define out-sort-value    (open-output-file fname-sort-value))
  (define out-sort-offset   (open-output-file fname-sort-offset))
  (define ms (multi-sorter out-sort-value out-sort-offset type value<))
  (method-lambda
    ((put! value) (ms 'put! value))
    ((close)
     (match-define (vector initial-item-count chunk-count v?) (ms 'close))
     (close-output-port out-sort-value)
     (close-output-port out-sort-offset)
     (define omax (if v? (sizeof `#(array ,initial-item-count ,type) v?)
                    (file-size fname-sort-value)))
     (define otype (and out-offset (nat-type/max omax)))
     (define item-count
       (cond (v? (let loop ((prev #f) (i 0) (count 0))
                   (if (= i initial-item-count) count
                     (let ((x (vector-ref v? i)))
                       (cond ((not (and dedup? (< 0 i) (equal? x prev)))
                              (when out-offset
                                (encode out-offset otype (file-position
                                                           out-value)))
                              (encode out-value type x)
                              (loop x (+ i 1) (+ count 1)))
                             (else (loop x (+ i 1) count)))))))
             (else (let/files ((in fname-sort-value)
                               (in-offset fname-sort-offset)) ()
                     (multi-merge dedup? out-value out-offset type otype value<
                                  chunk-count in in-offset)))))
     (delete-file fname-sort-value)
     (delete-file fname-sort-offset)
     (close-output-port out-value)
     (when out-offset (close-output-port out-offset))
     (cons otype item-count))))

(define (multi-sorter out-chunk out-offset type value<)
  (define buffer-size (current-config-ref 'buffer-size))
  (let ((v (make-vector buffer-size)) (chunk-count 0) (item-count 0) (i 0))
    (method-lambda
      ((put! value) (vector-set! v i value)
                    (set! i (+ i 1))
                    (when (= i buffer-size)
                      (vector-sort! v value<)
                      (for ((x (in-vector v))) (encode out-chunk type x))
                      (encode out-offset 'nat (file-position out-chunk))
                      (set! item-count  (+ item-count buffer-size))
                      (set! chunk-count (+ chunk-count 1))
                      (set! i           0)))
      ((close) (vector-sort! v value< 0 i)
               (cond ((and (< 0 chunk-count) (= i 0))
                      (vector item-count chunk-count #f))
                     ((< 0 chunk-count)
                      (for ((i (in-range i)))
                        (encode out-chunk type (vector-ref v i)))
                      (encode out-offset 'nat (file-position out-chunk))
                      (vector (+ item-count i) (+ chunk-count 1) #f))
                     (else (vector i 0 v)))))))

(define (multi-merge
          dedup? out out-offset type otype v< chunk-count in in-offset)
  (define (s< sa sb) (v< (car sa) (car sb)))
  (define (s-chunk pos end)
    (cond ((<= end pos) '())
          (else (file-position in pos)
                (cons (decode in type) (let ((pos (file-position in)))
                                         (thunk (s-chunk pos end)))))))
  (define heap (make-vector chunk-count))
  (let loop ((hi 0) (start 0)) (when (< hi chunk-count)
                                 (define end (decode in-offset 'nat))
                                 (vector-set! heap hi (s-chunk start end))
                                 (loop (+ hi 1) end)))
  (heap! s< heap chunk-count)
  (let loop ((prev #f) (i 0) (hend chunk-count))
    (if (= hend 0) i
      (let* ((top (heap-top heap)) (x (car top)) (top (s-force (cdr top))))
        (loop x (cond ((not (and dedup? (< 0 i) (equal? x prev)))
                       (when out-offset (encode out-offset otype
                                                (file-position out)))
                       (encode out type x)
                       (+ i 1))
                      (else i))
              (cond ((null? top) (heap-remove!  s< heap hend)  (- hend 1))
                    (else        (heap-replace! s< heap hend top) hend)))))))

(define (heap-top h) (vector-ref h 0))
(define (heap! ? h end)
  (let loop ((i (- (quotient end 2) 1)))
    (when (<= 0 i) (heap-sink! ? h end i) (loop (- i 1)))))
(define (heap-remove! ? h end)
  (vector-set! h 0 (vector-ref h (- end 1))) (heap-sink! ? h (- end 1) 0))
(define (heap-replace! ? h end top)
  (vector-set! h 0 top)                      (heap-sink! ? h    end    0))
(define (heap-sink! ? h end i)
  (let loop ((i i))
    (let ((ileft (+ i i 1)) (iright (+ i i 2)))
      (cond ((<= end ileft))  ;; done
            ((<= end iright)
             (let ((p (vector-ref h i)) (l (vector-ref h ileft)))
               (when (? l p) (vector-set! h i l) (vector-set! h ileft p))))
            (else (let ((p (vector-ref h i))
                        (l (vector-ref h ileft)) (r (vector-ref h iright)))
                    (cond ((? l p) (cond ((? r l) (vector-set! h i r)
                                                  (vector-set! h iright p)
                                                  (loop iright))
                                         (else (vector-set! h i l)
                                               (vector-set! h ileft p)
                                               (loop ileft))))
                          ((? r p) (vector-set! h i r)
                                   (vector-set! h iright p)
                                   (loop iright)))))))))
(define (heap-add! ? h end v)
  (let loop ((i end))
    (if (= i 0) (vector-set! h i v)
      (let* ((iparent (- (quotient (+ i 1) 2) 1))
             (pv      (vector-ref h iparent)))
        (cond ((? v pv) (vector-set! h i pv) (loop iparent))
              (else     (vector-set! h i v)))))))

(define (alist-ref alist key (default (void)))
  (define kv (assoc key alist))
  (cond (kv              (cdr kv))
        ((void? default) (error "missing key in association list:" key alist))
        (else            default)))
(define (alist-remove alist key)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))
(define (alist-update alist key v->v (default (void)))
  (let loop ((kvs alist) (prev '()))
    (cond ((null?        kvs     )
           (when (void? default) (error "missing key in association list:" key alist))
           (cons (cons key (v->v default)) alist))
          ((equal? (caar kvs) key) (foldl cons (cons (cons key (v->v (cdar kvs))) (cdr kvs)) prev))
          (else                    (loop (cdr kvs) (cons (car kvs) prev))))))
(define (alist-set alist key value)
  (alist-update alist key (lambda (_) value) #f))

(define (list-arranger input-names output-names)
  (define ss.in    (generate-temporaries input-names))
  (define name=>ss (make-immutable-hash (map cons input-names ss.in)))
  (define ss.out   (map (lambda (n) (hash-ref name=>ss n)) output-names))
  (eval-syntax #`(lambda (row)
                   (match-define (list #,@ss.in) row)
                   (list #,@ss.out))))

(define (table-materializer source-names path.dir fprefix
                            column-names column-types key-name)
  (define t (tabulator path.dir fprefix column-names column-types key-name))
  (define transform (list-arranger source-names column-names))
  (method-lambda
    ((put! x) (t 'put! (transform x)))
    ((close)  (t 'close))))

(define (materialize-index-tables!
          path.dir source-fprefix name->type source-names index-descriptions)
  (cond ((null? index-descriptions) '())
        (else
          (define threshold (current-config-ref 'progress-logging-threshold))
          (define index-ms
            (map (lambda (td)
                   (define fprefix      (alist-ref td 'file-prefix))
                   (define column-names (alist-ref td 'column-names))
                   (define column-types (map name->type column-names))
                   (table-materializer source-names path.dir fprefix
                                       column-names column-types #f))
                 index-descriptions))
          (logf "Materializing ~s index table(s) from primary:\n"
                (length index-ms))
          (for-each (lambda (td) (pretty-write (alist-ref td 'column-names)))
                    index-descriptions)
          (let/files ((in (value-table-file-name source-fprefix))) ()
            (define src (s-decode in (map name->type (cdr source-names))))
            (time (s-each (lambda (x)
                            (let ((count (car x)))
                              (when (= 0 (remainder count threshold))
                                (logf "ingested ~s rows\n" count))
                              (for-each (lambda (m) (m 'put! x)) index-ms)))
                          (s-enumerate 0 src))))
          (logf "Processing all rows\n")
          (map (lambda (m) (time (m 'close))) index-ms))))

(define (valid-key-type? t)
  (match t
    ((or #f 'nat 'number `#(nat ,_)) #t)
    (_                               #f)))

(define (materializer path.dir source-info attribute-names attribute-types key
                      table-descriptions)
  (define (unique?! as) (unless (= (length (remove-duplicates as)) (length as))
                          (error "duplicates:" as)))
  (unique?! attribute-names)
  (unless (= (length attribute-names) (length attribute-types))
    (error "mismatching attribute names and types:"
           attribute-names attribute-types))
  (define name=>type (make-immutable-hash
                       (map cons attribute-names attribute-types)))
  (when (null? table-descriptions)
    (error "empty list of table descriptions for:" attribute-names))
  (define index-tds            (cdr table-descriptions))
  (define primary-column-names (car table-descriptions))
  (define primary-source-names (cons key primary-column-names))
  (unique?! primary-column-names)
  (when (member key primary-column-names)
    (error "key name must be distinct:" key primary-column-names))
  (unless (set=? (set-remove attribute-names key) primary-column-names)
    (error "primary columns must include all non-key attributes:"
           (set->list (set-remove (list->set attribute-names) key))
           (set->list (list->set primary-column-names))))
  (define primary-column-types (map (lambda (n) (hash-ref name=>type n))
                                    primary-column-names))
  (unless (valid-key-type? (hash-ref name=>type key #f))
    (error "invalid key type:" (hash-ref name=>type key #f) key))
  (make-directory* path.dir)
  (define path.metadata (path->string (build-path path.dir metadata-file-name)))
  (define primary-fprefix "primary")
  (define primary-fname (path->string (build-path path.dir primary-fprefix)))
  (define index-fprefixes
    (map (lambda (i) (string-append "index." (number->string i)))
         (range (length index-tds))))
  (define primary-t (tabulator path.dir primary-fprefix
                               primary-column-names primary-column-types key))
  (method-lambda
    ((put! x) (primary-t 'put! x))
    ((close) (define primary-info (primary-t 'close))
             (define key-type (nat-type/max (alist-ref primary-info 'length)))
             (define name->type
               (let ((name=>type (hash-set name=>type key key-type)))
                 (lambda (n) (hash-ref name=>type n))))
             (define index-infos
               (materialize-index-tables!
                 path.dir primary-fname name->type primary-source-names
                 (map (lambda (fprefix td) `((file-prefix . ,fprefix)
                                             (column-names . ,td)))
                      index-fprefixes index-tds)))
             (write-metadata path.metadata attribute-names attribute-types
                             primary-info index-infos source-info))))

(define (write-metadata path attribute-names attribute-types primary-info index-infos source-info)
  (let/files () ((out.metadata path))
    (pretty-write `((metadata-format-version . ,metadata-format-version.latest)
                    (attribute-names         . ,attribute-names)
                    (attribute-types         . ,attribute-types)
                    (primary-table           . ,primary-info)
                    (index-tables            . ,index-infos)
                    (source-info             . ,source-info))
                  out.metadata)))

(define (update-metadata path info)
  (define path.backup (string-append path ".backup"))
  (when (file-exists? path.backup)
    (error "backup path already exists:" path.backup))
  (rename-file-or-directory path path.backup)
  (apply write-metadata path
         (map (lambda (k) (hash-ref info k))
              '(attribute-names attribute-types primary-table index-tables source-info)))
  (delete-file path.backup))

(define (metadata/2020-12-19.0 info)
  (define source-info (make-immutable-hash (hash-ref info 'source-info)))
  (define source-info.new
    (foldl (lambda (k v source-info) (hash-set source-info k v))
           (hash-remove source-info 'transform)
           '(map/append map filter)
           (list (hash-ref source-info 'map/append #f)
                 (hash-ref source-info 'map        (hash-ref source-info 'transform #f))
                 (hash-ref source-info 'filter     #f))))
  (foldl (lambda (k v info) (hash-set info k v))
         info
         '(metadata-format-version source-info)
         `(2021-03-18.0
           ,(if (hash-has-key? source-info.new 'path)
              (map (lambda (k) (cons k (hash-ref source-info.new k)))
                   '(path format header stats map/append map filter))
              (map (lambda (k) (cons k (hash-ref source-info.new k)))
                   '(stream map/append map filter))))))

(define (metadata/2021-03-18.0 info)
  ;; TODO: define an appropriate transformtion once this is no longer the latest version
  info)

;; TODO: when new metadata formats are introduced, update the old handlers to
;; have them transform an instance of the old format into an instance of the
;; newest format.  This can be done by composing handlers.

;; TODO: register new metadata-format-version handlers here
(define metadata-format-version.latest '2021-03-18.0)
(define metadata/format-version
  (hash '2021-03-18.0 metadata/2021-03-18.0
        '2020-12-19.0 metadata/2020-12-19.0))

(define (read-metadata path)
  (define info.0 (let/files ((in path)) () (read in)))
  (when (eof-object? info.0) (error "corrupt relation metadata:" path))
  (define info.1 (make-immutable-hash info.0))
  (define info ((hash-ref metadata/format-version
                          (hash-ref info.1 'metadata-format-version '2020-12-19.0))
                info.1))
  (define diff
    (foldl (lambda (k diff)
             (cond ((not (hash-has-key? info   k)) (cons `(,k old: ,(hash-ref info.1 k)) diff))
                   ((not (hash-has-key? info.1 k)) (cons `(,k new: ,(hash-ref info   k)) diff))
                   (else (define v.new (hash-ref info   k))
                         (define v.old (hash-ref info.1 k))
                         (if (equal? v.new v.old) diff
                           (cons `(,k old: ,v.old new: ,v.new) diff)))))
           '()
           (set->list (set-union (list->set (hash-keys info))
                                 (list->set (hash-keys info.1))))))
  (define should-update?
    (and (not (null? diff))
         (policy-allow?
           (current-config-ref 'update-policy)
           (lambda ()
             (printf "Current ~s is written in an old format:\n" path)
             (for-each pretty-write diff))
           "Update ~s to the latest format?" (list path))))
  (when should-update? (update-metadata path info))
  info)

(define (update-materialization! path.dir info tables.added tables.removed)
  (define path.metadata        (path->string
                                 (build-path path.dir metadata-file-name)))
  (define path.metadata.backup (string-append path.metadata ".backup"))
  (define attribute-names      (hash-ref info 'attribute-names))
  (define attribute-types      (hash-ref info 'attribute-types))
  (define source-info          (hash-ref info 'source-info))
  (define primary-info         (hash-ref info 'primary-table))
  (define index-infos          (hash-ref info 'index-tables))
  (define primary-key-name     (alist-ref primary-info 'key-name))
  (define primary-column-names (alist-ref primary-info 'column-names))
  (define source-fprefix
    (path->string (build-path path.dir (alist-ref primary-info 'file-prefix))))
  (define source-names (cons primary-key-name primary-column-names))
  (define key-type (nat-type/max (alist-ref primary-info 'length)))
  (define name=>type (make-immutable-hash
                       (cons (cons primary-key-name key-type)
                             (map cons attribute-names attribute-types))))
  (define (name->type n) (hash-ref name=>type n))
  (define cols=>info
    (make-immutable-hash
      (map (lambda (info.it) (cons (alist-ref info.it 'column-names) info.it))
           (hash-ref info 'index-tables))))
  (define index-infos.current
    (hash-values (foldl (lambda (cols c=>i) (hash-remove c=>i cols))
                        cols=>info tables.removed)))
  (rename-file-or-directory path.metadata path.metadata.backup)
  (for-each
    (lambda (cols)
      (define info    (hash-ref cols=>info cols))
      (define fprefix (alist-ref info 'file-prefix))
      (define fname.v
        (path->string (build-path path.dir (value-table-file-name  fprefix))))
      (define fname.o
        (path->string (build-path path.dir (offset-table-file-name fprefix))))
      (logf "Deleting ~s\n" fname.v)
      (delete-file fname.v)
      (when (alist-ref info 'offset-file)
        (logf "Deleting ~s\n" fname.o)
        (delete-file fname.o)))
    tables.removed)
  (define index-descriptions.added
    (let loop ((colss tables.added) (i 0))
      (if (null? colss) '()
        (let ((fprefix (string-append "index." (number->string i))))
          (if (file-exists?
                (build-path path.dir (value-table-file-name fprefix)))
            (loop colss (+ i 1))
            (cons `((file-prefix  . ,fprefix) (column-names . ,(car colss)))
                  (loop (cdr colss) (+ i 1))))))))
  (define index-infos.new
    (materialize-index-tables! path.dir source-fprefix name->type
                               source-names index-descriptions.added))
  (write-metadata path.metadata attribute-names attribute-types
                  primary-info (append index-infos.current index-infos.new)
                  source-info)
  (delete-file path.metadata.backup))

(define (materialization/vector vector.in kwargs)
  (define info            (make-immutable-hash kwargs))
  (define name            (hash-ref info 'relation-name))
  (define sort?           (hash-ref info 'sort?  #t))
  (define dedup?          (hash-ref info 'dedup? #t))
  (define key             (hash-ref info 'key-name #t))
  (define attribute-names (hash-ref info 'attribute-names))
  (define attribute-types (hash-ref info 'attribute-types
                                    (map (lambda (n)
                                           (if (equal? n key) 'nat #f))
                                         attribute-names)))
  (define table-descriptions
    (append (hash-ref info 'tables `(,(remove key attribute-names)))
            (map (lambda (cols) (append cols (list key)))
                 (hash-ref info 'indexes '()))))
  (define name=>type.0 (make-immutable-hash
                         (map cons attribute-names attribute-types)))
  (unless (valid-key-type? (hash-ref name=>type.0 key 'nat))
    (error "invalid key type:" (hash-ref name=>type.0 key 'nat) kwargs))
  (define name=>type (hash-set name=>type.0 key 'nat))
  (define (name->type n) (hash-ref name=>type n))
  (define primary-column-names (car table-descriptions))
  (define primary-column-types (map name->type primary-column-names))
  (define primary-source-names (cons key primary-column-names))
  (unless (vector? vector.in)
    (error "invalid source vector:" kwargs vector.in))
  (when sort? (vector-table-sort! primary-column-types vector.in))
  (define primary-v (if dedup? (vector-dedup vector.in) vector.in))
  (define primary-t (table/vector key primary-column-names
                                  primary-column-types primary-v))
  (define index-ts
    (let* ((ss.sources (generate-temporaries primary-source-names))
           (name=>ss (make-immutable-hash
                       (map cons primary-source-names ss.sources)))
           (name->ss (lambda (n) (hash-ref name=>ss n))))
      (map (lambda (column-names)
             (define column-types (map name->type column-names))
             (define ss.columns   (map name->ss   column-names))
             (define index-src
               (let ((transform
                       (eval-syntax
                         #`(lambda (#,(car ss.sources) row)
                             (match-define (vector #,@(cdr ss.sources)) row)
                             (vector #,@ss.columns))))
                     (iv (make-vector (vector-length primary-v))))
                 (for ((i   (in-range (vector-length primary-v)))
                       (row (in-vector primary-v)))
                   (vector-set! iv i (transform i row)))
                 iv))
             (vector-table-sort! column-types index-src)
             (table/vector #f column-names column-types
                           (vector-dedup index-src)))
           (cdr table-descriptions))))
  (list (foldl (lambda (k v info) (hash-set info k v)) info
               '(key-name attribute-types table-descriptions missing-data?)
               (list key attribute-types table-descriptions (not (hash-ref info 'source-vector #f))))
        (cons primary-t index-ts)))

(define (materialization/path directory-path kwargs)
  (define name           (alist-ref kwargs 'relation-name))
  (define retrieval-type (alist-ref kwargs 'retrieval-type 'disk))
  (define path.dir       (current-config-relation-path directory-path))
  (unless (directory-exists? path.dir)
    (error "materialized relation directory does not exist:" path.dir))
  (define path.metadata  (path->string
                           (build-path path.dir metadata-file-name)))
  (define info           (read-metadata path.metadata))
  (define attribute-names    (hash-ref info 'attribute-names))
  (define attribute-types    (hash-ref info 'attribute-types))
  (define primary-info-alist (hash-ref info 'primary-table))
  (define primary-info       (make-immutable-hash primary-info-alist))
  (define primary-t          (table/metadata
                               retrieval-type path.dir primary-info-alist))
  (define primary-key-name   (hash-ref primary-info 'key-name))
  (define index-ts
    (map (lambda (info) (table/metadata retrieval-type path.dir info))
         (hash-ref info 'index-tables '())))
  (list (foldl (lambda (k v info) (hash-set info k v)) info
               '(relation-name key-name retrieval-type)
               (list name primary-key-name retrieval-type))
        (cons primary-t index-ts)))

(define (materialization . pargs)
  (define kwargs        (plist->alist pargs))
  (define path          (alist-ref kwargs 'path          #f))
  (define path.dir      (and path (current-config-relation-path path)))
  (define path.metadata (and path.dir (path->string (build-path path.dir metadata-file-name))))
  (define info          (and path.metadata (directory-exists? path.dir) (read-metadata path.metadata)))
  (define stream.source (alist-ref kwargs 'source-stream #f))
  (define path.source.0 (alist-ref kwargs 'source-file-path #f))
  (define path.source   (and path.source.0 (current-config-relation-path path.source.0)))
  (define vec.source
    (or (alist-ref kwargs 'source-vector #f)
        (and (not info)
             (not (alist-ref kwargs 'source-stream #f))
             (not (and path.source (file-exists? path.source)))
             (policy-allow?
               (current-config-ref 'allow-missing-data-policy)
               (lambda ()
                 (printf "Source (~s) and processed data are both missing for ~s:\n"
                         path.source.0 path))
               "Allow ~s to be used as an empty relation for now?" (list path))
             (vector))))
  (cond (vec.source (materialization/vector vec.source kwargs))
        (path       (when (or path.source.0 stream.source)
                      (apply materialize-relation! pargs))
                    (materialization/path path kwargs))
        (else       (error "missing relation path or source:" kwargs))))

(struct value+syntax (value syntax) #:prefab)
(define-syntax-rule (value/syntax e) (value+syntax e #'e))

(define (plist->alist kvs) (if (null? kvs) '()
                             (cons (cons (car kvs) (cadr kvs))
                                   (plist->alist (cddr kvs)))))
(define (materialize-relation! . pargs)
  (define kwargs          (plist->alist pargs))
  (define path            (alist-ref kwargs 'path))
  (define key-name        (alist-ref kwargs 'key-name #t))
  (define attribute-names (alist-ref kwargs 'attribute-names))
  (define attribute-types (alist-ref kwargs 'attribute-types
                                     (map (lambda (n)
                                            (if (equal? n key-name) 'nat #f))
                                          attribute-names)))
  (define fn.in           (alist-ref kwargs 'source-file-path    #f))
  (define format.in       (alist-ref kwargs 'source-file-format  #f))
  (define header.in       (alist-ref kwargs 'source-file-header  '()))
  (define names.in        (alist-ref kwargs 'source-file-columns
                                     (remove key-name attribute-names)))
  (define stream.in       (alist-ref kwargs 'source-stream       #f))
  (define map/append-code (alist-ref kwargs 'map/append          #f))
  (define map-code        (alist-ref kwargs 'map                 #f))
  (define filter-code     (alist-ref kwargs 'filter              #f))
  (unless key-name (error "key-name cannot be #f:" kwargs))
  (define table-descriptions
    (append (alist-ref kwargs 'tables `(,(remove key-name attribute-names)))
            (map (lambda (cols) (append cols (list key-name)))
                 (alist-ref kwargs 'indexes '()))))
  (define threshold (current-config-ref 'progress-logging-threshold))
  (define path.in   (and fn.in (current-config-relation-path fn.in)))
  (define path.dir  (current-config-relation-path path))
  (define path.log  (build-path path.dir "progress.log"))
  (define format    (or format.in (and fn.in (path->format fn.in))))
  (define header    (if (pair? header.in)
                      (map (lambda (s) (if (symbol? s) (symbol->string s) s))
                           header.in)
                      header.in))
  (define path.metadata (path->string (build-path path.dir metadata-file-name)))
  (define info          (and (directory-exists? path.dir) (read-metadata path.metadata)))

  (define (code->info code)
    (cond ((procedure? code)    #t)
          ((value+syntax? code) (syntax->datum (value+syntax-syntax code)))
          (else                 code)))
  (define (code->value code)
    (cond ((value+syntax? code) (value+syntax-value code))
          (else                 code)))
  (when (and map/append-code (or map-code filter-code))
    (error "map/append should not be used with map or filter:"
           `((map/append ,(code->info map/append-code))
             (map        ,(code->info map-code))
             (filter     ,(code->info filter-code)))))
  (define source-info
    (cond (path.in   `((path       . ,fn.in)
                       (format     . ,format)
                       (header     . ,header)
                       (stats      . ,(or (file-stats path.in)
                                          (and info (alist-ref (hash-ref info 'source-info '())
                                                               'stats #f))))
                       (map/append . ,(code->info map/append-code))
                       (map        . ,(code->info map-code))
                       (filter     . ,(code->info filter-code))))
          (stream.in `((stream     . ,(code->info stream.in))
                       (map/append . ,(code->info map/append-code))
                       (map        . ,(code->info map-code))
                       (filter     . ,(code->info filter-code))))
          (else (error "materialize-relation! missing file or stream source:"
                       kwargs))))
  (define map/append? (code->value map/append-code))
  (define map?        (code->value map-code))
  (define filter?     (code->value filter-code))
  (define (materialize-stream source-info stream)
    (let ((mat (materializer path.dir source-info
                             attribute-names attribute-types key-name
                             table-descriptions)))
      (define count 0)
      (define arrange (list-arranger names.in (car table-descriptions)))
      (time (s-each (lambda (x)
                      (when (= 0 (remainder count threshold))
                        (logf "ingested ~s rows\n" count))
                      (mat 'put! (arrange x))
                      (set! count (+ count 1)))
                    (if map/append?
                      (s-map/append map/append? stream)
                      (let ((s (if map? (s-map map? stream) stream)))
                        (if filter? (s-filter filter? s) s)))))
      (logf "Processing ~s rows\n" count)
      (time (mat 'close))
      (logf "Finished processing ~s rows\n" count)))
  (define (materialize-source)
    (if path.in
      (let/files ((in path.in)) ()
        (logf/date "Materializing relation ~s from ~s file ~s\n"
                   path format fn.in)
        (define stream ((format->header/port->stream format) header in))
        (materialize-stream source-info stream))
      (begin (logf/date "Materializing relation ~s from stream\n" path)
             (materialize-stream source-info (code->value stream.in)))))

  (if info
    (let* ((source-info.old     (hash-ref info 'source-info #f))
           (attribute-names.old (hash-ref info 'attribute-names))
           (attribute-types.old (hash-ref info 'attribute-types))
           (primary-table.old   (hash-ref info 'primary-table))
           (key-name.old        (alist-ref primary-table.old 'key-name))
           (primary-columns.old (alist-ref primary-table.old 'column-names))
           (primary-columns.new (car table-descriptions)))
      (unless (equal? attribute-names attribute-names.old)
        (error "new relation attributes are incompatible with old:" path
               'new: attribute-names 'old: attribute-names.old))
      (define stale-fields.0
        (map (lambda (desc)
               (match-define (list key new old) desc)
               `(,key new: ,new old: ,old))
             (filter
               (lambda (desc)
                 (match-define (list _ new old) desc)
                 (not (equal? new old)))
               `((source-info     ,source-info     ,source-info.old)
                 (attribute-types ,attribute-types ,attribute-types.old)
                 (key-name        ,key-name        ,key-name.old)
                 (primary-table   ,primary-columns.new
                                  ,primary-columns.old)))))
      (define update-policy  (current-config-ref 'update-policy))
      (define cleanup-policy (current-config-ref 'cleanup-policy))
      (define stale-fields
        (if (and (equal? '(source-info) (map car stale-fields.0))
                 (policy-allow?
                   update-policy
                   (lambda ()
                     (printf "Existing source-info for relation ~s is stale:\n" path)
                     (for-each pretty-write stale-fields.0))
                   "Update ~s metadata with new source-info without rematerializing?" (list path)))
          (begin (printf "Updating ~s" path.metadata)
                 (update-metadata path.metadata (hash-set info 'source-info source-info))
                 (printf "Update of ~s finished" path.metadata)
                 '())
          stale-fields.0))
      (define allow-update?
        (or (null? stale-fields)
            (policy-allow?
              update-policy
              (lambda ()
                (printf "Existing data for relation ~s is stale:\n" path)
                (for-each pretty-write stale-fields))
              "Update ~s?" (list path))))
      (cond ((and (pair? stale-fields) allow-update?)
             (printf "Updating ~s\n" path)
             (define path.backup (string-append path.dir ".backup"))
             (when (directory-exists? path.backup)
               (error "backup path already exists:" path.backup))
             (rename-file-or-directory path.dir path.backup)
             (materialize-source)
             (printf "Rematerialization of ~s finished\n" path)
             (when (directory-exists? path.backup)
               (if (or (equal? cleanup-policy 'always)
                       (and (equal? cleanup-policy 'interactive)
                            (printf "Delete backup directory ~s? [y/n]: "
                                    path.backup)
                            (case (read)
                              ((y Y yes Yes YES) #t)
                              (else              #f))))
                 (begin (printf "Deleting ~s\n" path.backup)
                        (delete-directory/files path.backup #:must-exist? #f))
                 (printf "Not deleting ~s\n" path.backup))))
            ((not allow-update?)
             (printf "Due to stale data, will not rematerialize ~s\n" path))
            (else
              (define colss.current
                (map (lambda (info.it) (alist-ref info.it 'column-names))
                     (hash-ref info 'index-tables)))
              (define colss.new (cdr table-descriptions))
              (define (cols-current? cols) (member cols colss.current))
              (define (cols-new?     cols) (member cols colss.new))
              (define added   (filter-not cols-current? colss.new))
              (define removed (filter-not cols-new?     colss.current))
              (define add?
                (and (pair? added)
                     (policy-allow?
                       update-policy
                       (lambda ()
                         (printf "New index tables included for relation ~s:\n"
                                 path)
                         (for-each pretty-write added))
                       "Add these new index tables to ~s?"
                       (list path))))
              (define remove?
                (and (pair? removed)
                     (policy-allow?
                       cleanup-policy
                       (lambda ()
                         (printf
                           "Index tables no longer included for relation ~s:\n"
                           path)
                         (for-each pretty-write removed))
                       "Remove these old index tables from ~s?"
                       (list path))))
              (when (or add? remove?)
                (update-materialization! path.dir info
                                         (if add?    added   '())
                                         (if remove? removed '()))))))
    (materialize-source)))
