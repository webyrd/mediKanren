#lang racket/base
(provide bisect bisect-next
         table/metadata table/vector table/bytes table/port
         table/bytes/offsets table/port/offsets sorter tabulator encoder
         table-project table-intersect-start table-cross table-join
         value-table-file-name offset-table-file-name
         call/files let/files s-encode s-decode)
(require "codec.rkt" "method.rkt" "order.rkt" "stream.rkt"
         racket/file racket/function racket/list racket/match racket/set
         racket/vector)

(define (s-encode out type s) (s-each s (lambda (v) (encode out type v))))
(define (s-decode in type)
  (thunk (let loop () (if (eof-object? (peek-byte in)) '()
                        (cons (decode in type) (thunk (loop)))))))

(define (encoder out type) (method-lambda ((put v) (encode out type v))
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

(define (table vref cols types mask start end)
  (define (ref i j) (vector-ref  (vref i) (+ mask j)))
  (define (ref* i)  (vector-copy (vref i)    mask))
  (define compare (and (pair? types) (type->compare (car types))))
  (define <?      (and compare (compare-><? compare)))
  (define <=?     (and compare (compare-><=? compare)))
  (define (make-i<  v) (lambda (i) (<?  (ref i 0) v)))
  (define (make-i<= v) (lambda (i) (<=? (ref i 0) v)))
  (define (make-i>  v) (lambda (i) (<?  v (ref i 0))))
  (define (make-i>= v) (lambda (i) (<=? v (ref i 0))))
  (method-lambda
    ((columns) cols)
    ((types)   types)
    ((width)   (length cols))
    ((length)  (- end start))
    ((key)     start)
    ((ref* i)  (ref* (+ start i)))
    ((ref i j) (ref  (+ start i) j))
    ((mask j)  (table vref (s-drop j cols) (s-drop j types)
                      (+ mask j) start end))
    ((stream)  (let loop ((i 0))
                 (thunk (if (= i (- end start)) '()
                          (cons (vector->list (ref* (+ start i)))
                                (loop (+ i 1)))))))
    ((find<  v) (bisect start end (make-i< v)))
    ((find<= v) (bisect start end (make-i<= v)))
    ((drop<  v) (table vref cols types mask
                       (bisect-next start end (make-i< v)) end))
    ((drop<= v) (table vref cols types mask
                       (bisect-next start end (make-i<= v)) end))
    ((take<= v) (table vref cols types mask
                       start (bisect-next start end (make-i<= v))))
    ;; TODO: > >= variants: take>= drop> drop>=
    ;((drop> v)  (table vref cols types mask
                       ;start (bisect-prev start end (make-i> v))))
    ((drop-key< k) (table vref cols types mask (max start (min k end)) end))
    ((drop-key> k) (table vref cols types mask start (min end (max k start))))
    ((take count) (table vref cols types mask start           (+ count start)))
    ((drop count) (table vref cols types mask (+ count start) end))))

(define (table/port/offsets table.offsets cols types in)
  (define type `#(tuple ,@types))
  (define (ref i)
    (file-position in (table.offsets 'ref i 0))
    (decode in type))
  (table ref cols types 0 0 (table.offsets 'length)))

(define (table/bytes/offsets table.offsets cols types bs)
  (define in (open-input-bytes bs))
  (table/port/offsets table.offsets cols types in))

;; TODO: table/file that does len calculation via file-size?
(define (table/port len cols types in)
  (define type `#(tuple ,@types))
  (define width (sizeof type (void)))
  (define (ref i) (file-position in (* i width)) (decode in type))
  (table ref cols types 0 0 len))

(define (table/bytes cols types bs)
  (define in (open-input-bytes bs))
  (table/port (quotient (bytes-length bs) (sizeof types (void)))
              cols types in))

(define (table/vector cols types v)
  (table (lambda (i) (vector-ref v i)) cols types 0 0 (vector-length v)))

(define (table/metadata retrieval-type file-prefix info-alist)
  ;(define (warning . args) (printf "warning: ~s\n" args))
  (define (warning . args) (error "warning:" args))
  (define fname.value  (value-table-file-name  file-prefix))
  (define fname.offset (offset-table-file-name file-prefix))
  (define info (make-immutable-hash info-alist))
  (define offset-type  (hash-ref info 'offset-type))
  (define column-names (hash-ref info 'column-names))
  (define column-types (hash-ref info 'column-types))
  (define len (hash-ref info 'length))
  (unless (equal? (file-size fname.value) (hash-ref info 'value-file-size))
    (error "file size does not match metadata:" fname.value
           (file-size fname.value) (hash-ref info 'value-file-size)))
  (unless (equal? (file-or-directory-modify-seconds fname.value)
                  (hash-ref info 'value-file-time))
    (warning "file modification time does not match metadata:" fname.value
             (file-or-directory-modify-seconds fname.value)
             (hash-ref info 'value-file-time)))
  (when offset-type
    (unless (equal? (file-size fname.offset) (hash-ref info 'offset-file-size))
      (error "file size does not match metadata:" fname.offset
             (file-size fname.offset) (hash-ref info 'offset-file-size)))
    (unless (equal? (file-or-directory-modify-seconds fname.offset)
                    (hash-ref info 'offset-file-time))
      (warning "file modification time does not match metadata:" fname.offset
               (file-or-directory-modify-seconds fname.offset)
               (hash-ref info 'offset-file-time))))
  (define t.value
    (case retrieval-type
      ((disk) (define in.value (open-input-file fname.value))
              (if offset-type
                (table/port/offsets (table/port len '(offset) `(,offset-type)
                                                (open-input-file fname.offset))
                                    column-names column-types in.value)
                (table/port len column-names column-types in.value)))
      ((bytes) (define bs.value (file->bytes fname.value))
               (if offset-type
                 (table/bytes/offsets (table/bytes '(offset) `(,offset-type)
                                                   (file->bytes fname.offset))
                                      column-names column-types bs.value)
                 (table/bytes column-types bs.value)))
      ((scm) (let/files ((in.value fname.value)) ()
               (table/vector
                 column-names column-types
                 (list->vector
                   (s-take #f (s-decode in.value `#(tuple ,@column-types)))))))
      (else (error "unknown retrieval type:" retrieval-type))))
  t.value)

(define (bisect start end i<)
  (let loop ((start start) (end end))
    (if (<= end start) end
      (let ((i (+ start (quotient (- end start) 2))))
        (if (i< i) (loop (+ 1 i) end) (loop start i))))))

(define (bisect-next start end i<)
  (define i (- start 1))
  (let loop ((offset 1))
    (define next (+ i offset))
    (cond ((and (< next end) (i< next))
           (loop (arithmetic-shift offset 1)))
          (else (let loop ((i i) (o offset))
                  (let* ((o (arithmetic-shift o -1)) (next (+ i o)))
                    (cond ((= o 0)                      (+ i 1))
                          ((and (< next end) (i< next)) (loop next o))
                          (else                         (loop i    o)))))))))
;; TODO: bisect-prev

(define (table-project t v) (((t 'drop< v) 'take<= v) 'mask 1))

;; TODO: table-intersect-end
(define (table-intersect-start ts)
  (define (next t) (and (< 0 (t 'length)) (list (t 'ref 0 0))))
  (define initial-max (and (not (null? ts)) (next (car ts))))
  (and initial-max
       (let loop ((max (car initial-max)) (ts ts) (finished '()))
         (if (null? ts)
           (let loop ((max max) (pending (reverse finished)) (finished '()))
             (if (null? pending) (loop max (reverse finished) '())
               (let ((t-next (caar pending)) (t (cdar pending)))
                 (if (equal? t-next max)
                   (cons max (map cdr (foldl cons pending finished)))
                   (let* ((t (t 'drop< max)) (new (next t)))
                     (and new (loop (car new) (cdr pending)
                                    (cons (cons (car new) t) finished))))))))
           (let* ((t ((car ts) 'drop< max)) (new (next t)))
             (and new (loop (car new) (cdr ts) (cons (cons (car new) t)
                                                     finished))))))))

;; TODO: this may only be useful as an example.
(define (table-cross ts)
  (foldl (lambda (t suffixes)
           (append* (map (lambda (row)
                           (map (lambda (suffix) (append row suffix))
                                suffixes))
                         (s-take #f (t 'stream)))))
         '(()) (reverse ts)))

;; TODO: this may only be useful as an example.
(define (table-join ts prefix-size)
  (let outer-loop ((psize prefix-size) (ts ts))
    (if (= psize 0) (table-cross ts)
      (let inner-loop ((ts ts))
        (let ((v+ts (table-intersect-start ts)))
          (cond ((not v+ts) '())
                (else (define v  (car v+ts))
                      (define ts (cdr v+ts))
                      (define (current t) ((t 'take<= v) 'mask 1))
                      (define (next    t) (t 'drop<= v))
                      (append (map (lambda (row) (cons v row))
                                   (outer-loop (- psize 1) (map current ts)))
                              (inner-loop (map next ts))))))))))

(define (value-table-file-name  prefix) (string-append prefix ".value.table"))
(define (offset-table-file-name prefix) (string-append prefix ".offset.table"))

(define (tabulator source-names buffer-size file-prefix
                   column-names column-types key-name sorted-columns)
  (define (unique?! as) (unless (= (length (remove-duplicates as)) (length as))
                          (error "duplicates:" as)))
  (unless (= (length column-names) (length column-types))
    (error "mismatching column names and types:" column-names column-types))
  (unique?! source-names)
  (unique?! column-names)
  (unless (andmap symbol? column-names)
    (error "column names must be symbols:" column-names))
  (when (or (member key-name column-names) (member key-name source-names))
    (error "key name must be distinct:" key-name column-names source-names))
  (unless (subset? column-names source-names)
    (error "column names not covered by source:" column-names source-names))
  (unless (subset? sorted-columns column-names)
    (error "unknown sorted column names:" sorted-columns column-names))
  (define column-ixs (map (lambda (a) (index-of source-names a)) column-names))
  (define row-type column-types)  ;; TODO: possibly change this to tuple?
  (define row<     (compare-><? (type->compare row-type)))
  (define row-size (sizeof row-type (void)))
  (make-parent-directory* file-prefix)
  (define value-file-name  (value-table-file-name file-prefix))
  (define offset-file-name (and (not row-size)
                                (offset-table-file-name file-prefix)))
  (define tsorter (sorter #t value-file-name offset-file-name buffer-size
                          row-type row<))
  (method-lambda
    ((put x) (tsorter 'put (map (lambda (ix) (list-ref x ix)) column-ixs)))
    ((close) (match-define (cons offset-type item-count) (tsorter 'close))
             `((value-file-size   . ,(file-size value-file-name))
               (value-file-time   . ,(file-or-directory-modify-seconds
                                       value-file-name))
               (offset-file-size  . ,(and offset-file-name
                                          (file-size offset-file-name)))
               (offset-file-time  . ,(and offset-file-name
                                          (file-or-directory-modify-seconds
                                            offset-file-name)))
               (offset-type       . ,offset-type)
               (length            . ,item-count)
               (column-names      . ,column-names)
               (column-types      . ,column-types)
               (key-name          . ,key-name)
               (sorted-columns    . ,sorted-columns)))))

(define (sorter dedup? value-file-name offset-file-name? buffer-size
                type value<)
  (define fname-sort-value  (string-append value-file-name ".value.sort"))
  (define fname-sort-offset (string-append value-file-name ".offset.sort"))
  (define out-value         (open-output-file value-file-name))
  (define out-offset
    (and offset-file-name?  (open-output-file offset-file-name?)))
  (define out-sort-value    (open-output-file fname-sort-value))
  (define out-sort-offset   (open-output-file fname-sort-offset))
  (define ms (multi-sorter out-sort-value out-sort-offset buffer-size
                           type value<))
  (method-lambda
    ((put value) (ms 'put value))
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

(define (checked-file-position chunk i item-count chunk-count)
  (let ((fp (file-position chunk)))
    (if (void? fp)
	(error "file-position returned void:" i item-count chunk-count)
	fp)))

(define (multi-sorter out-chunk out-offset buffer-size type value<)
  (let ((v (make-vector buffer-size)) (chunk-count 0) (item-count 0) (i 0))
    (method-lambda
      ((put value) (vector-set! v i value)
                   (set! i (+ i 1))
                   (when (= i buffer-size)
                     (vector-sort! v value<)
                     (for ((x (in-vector v))) (when (void? x) (error "x is void:" type i item-count chunk-count)) (encode out-chunk type x))
                     (encode out-offset 'nat (checked-file-position out-chunk i item-count chunk-count))
                     (set! item-count  (+ item-count buffer-size))
                     (set! chunk-count (+ chunk-count 1))
                     (set! i           0)))
      ((close) (vector-sort! v value< 0 i)
               (cond ((< 0 chunk-count)
                      (vector-sort! v value< 0 i)
                      (for ((i (in-range i)))
                        (encode out-chunk type (vector-ref v i)))
                      (encode out-offset 'nat (file-position out-chunk))
                      (vector (+ item-count i) (+ chunk-count 1) #f))
                     (else (vector i 0 v)))))))

;; TODO: separate chunk streaming from merging
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
