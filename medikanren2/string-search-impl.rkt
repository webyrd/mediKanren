#lang racket/base
(provide
  string/searchable
  suffix:corpus->index
  suffix:corpus-find*
  suffix:corpus-find*/disk
  string:corpus->index
  string:corpus-find*
  string:corpus-find*/disk
  (prefix-out test: string/searchable)
  (prefix-out test: suffix-key-count/port)
  (prefix-out test: suffix-index->suffix-key)
  (prefix-out test:  suffix-key-count/port)
  (prefix-out test:  suffix-index->suffix-key)
  (prefix-out test: bytes->suffix-key)
  (prefix-out test: suffix-key->bytes)
  (prefix-out test: bytes->string-key)
  (prefix-out test: string-key->bytes)
  param-fd-input-binary
  ensure-fd-input-binary
  test:parameterize-defaults

  ;; The following are internals used in string-search.rkt
  vector-sparse-find
  suffix<?/corpus2
  concept-name
  write-scm
  write-suffix-keys
  )
(require
  racket/list
  (except-in racket/match ==)
  racket/string
  racket/unsafe/ops
  racket/vector
  racket/dict
  "base.rkt"
)

;; adapted from medikanren/repr.rkt
(define (concept-cui c)      (list-ref c 0))
(define (concept-name c)     (list-ref c 1))

;; BEGIN: excerpted from medikanren/repr.rkt
(define (byte-at offset n) (bitwise-and 255 (arithmetic-shift n offset)))
;; ...
(define string-key-byte-size 6)
(define (string-key->bytes cid)
  (bytes (byte-at -40 cid) (byte-at -32 cid) (byte-at -24 cid) (byte-at -16 cid) (byte-at -8 cid) (byte-at 0 cid)))
(define (bytes->string-key bs)
  (define (bref-to pos offset) (arithmetic-shift (bytes-ref bs pos) offset))
  (+ (bref-to 0 40) (bref-to 1 32) (bref-to 2 24) (bref-to 3 16) (bref-to 4 8) (bref-to 5 0)))
(define (read-string-key-bytes in) (read-bytes string-key-byte-size in))
(define (write-string-keys out v)
  (for ((s (in-vector v))) (write-bytes (string-key->bytes s) out)))
(define (port->string-keys in)
  (define end (begin (file-position in eof) (file-position in)))
  (file-position in 0)
  (for/vector ((_ (in-range (/ end string-key-byte-size))))
              (bytes->string-key (read-string-key-bytes in))))
(define (string-index->string-key in si)
  (file-position in (* string-key-byte-size si))
  (bytes->string-key (read-string-key-bytes in)))
(define (string-key-count in)
  (file-position in eof)
  (/ (file-position in) string-key-byte-size))

(define suffix-key-byte-size (+ 6 2))
(define (suffix-key-count bs) (/ (bytes-length bs) suffix-key-byte-size))
(define (suffix-key-count/port in)
  (file-position in eof)
  (/ (file-position in) suffix-key-byte-size))
(define (suffix-key-ref index i) (bytes->suffix-key index i))
(define (suffix-key->bytes s)
  (define cid (car s))
  (define pos (cdr s))
  (bytes (byte-at -40 cid) (byte-at -32 cid) 
         (byte-at -24 cid) (byte-at -16 cid) (byte-at -8 cid) (byte-at 0 cid)
         (byte-at -8 pos) (byte-at 0 pos)))
(define (bytes->suffix-key bs start)
  (define i (* start suffix-key-byte-size))
  (define (bref-to j offset) (arithmetic-shift (bytes-ref bs (+ i j)) offset))
  (cons (+ (bref-to 0 40) (bref-to 1 32) (bref-to 2 24) (bref-to 3 16) (bref-to 4 8) (bref-to 5 0))
        (+ (bref-to 6 8) (bref-to 7 0))))
(define (read-suffix-key-bytes in) (read-bytes suffix-key-byte-size in))
(define (write-suffix-keys out v)
  (for ((s (in-vector v))) (write-bytes (suffix-key->bytes s) out)))
(define (suffix-index->suffix-key in si)
  (file-position in (* suffix-key-byte-size si))
  (bytes->suffix-key (read-suffix-key-bytes in) 0))
;; ...
(define (write-scm out scm) (fprintf out "~s\n" scm))
;; END:excerpted from medikanren/repr.rkt

(define (msd-radix-sort vs len dref v<?)
  (define vvs (list->vector vs))
  (define out (make-vector (vector-length vvs)))
  (define pos-count 127)
  (define pos (make-vector pos-count))
  (define (++pos! i) (unsafe-vector*-set!
                       pos i (+ 1 (unsafe-vector*-ref pos i))))
  (let loop ((bi 0) (vi 0) (vend (vector-length vvs)) (vs vvs) (out out))
    (cond
      ((< (- vend vi) 100) (vector-sort! vs v<? vi vend)
                           (when (= 0 (bitwise-and bi 1))
                             (vector-copy! out vi vs vi vend)))
      (else (vector-fill! pos 0)
            (define done-pos
              (let i-loop ((i vi) (done-pos vi))
                (cond ((< i vend)
                       (define v (unsafe-vector*-ref vs i))
                       (cond ((<= (len v) bi)
                              (unsafe-vector*-set! out done-pos v)
                              (i-loop (+ 1 i) (+ 1 done-pos)))
                             (else (define d (dref v bi))
                                   (when (< d pos-count) (++pos! d))
                                   (i-loop (+ 1 i) done-pos))))
                      (else done-pos))))
            (define initial-big-pos
              (let p-loop ((d 0) (p done-pos))
                (cond ((< d pos-count)
                       (define p^ (+ p (unsafe-vector*-ref pos d)))
                       (unsafe-vector*-set! pos d p)
                       (p-loop (+ 1 d) p^))
                      (else p))))
            (define big-pos
              (let i-loop ((i vi) (big-pos initial-big-pos))
                (cond ((< i vend)
                       (define v (unsafe-vector*-ref vs i))
                       (cond ((< bi (len v))
                              (define d (dref v bi))
                              (cond ((< d pos-count)
                                     (unsafe-vector*-set!
                                       out (unsafe-vector*-ref pos d) v)
                                     (++pos! d)
                                     (i-loop (+ 1 i) big-pos))
                                    (else (unsafe-vector*-set! out big-pos v)
                                          (i-loop (+ 1 i) (+ 1 big-pos)))))
                             (else (i-loop (+ 1 i) big-pos))))
                      (else big-pos))))
            (when (< initial-big-pos vend)
              (vector-sort! out v<? initial-big-pos vend)
              (when (= 1 (bitwise-and bi 1))
                (vector-copy! vs initial-big-pos out initial-big-pos big-pos)))
            (when (= 1 (bitwise-and bi 1))
              (vector-copy! vs vi out vi done-pos))
            (let d-loop ((i done-pos))
              (when (< i initial-big-pos)
                (define d (dref (unsafe-vector*-ref out i) bi))
                (let end-loop ((end i) (offset 1))
                  (define next (+ end offset))
                  (cond
                    ((and (< next initial-big-pos)
                          (= d (dref (unsafe-vector*-ref out next) bi)))
                     (end-loop next (arithmetic-shift offset 1)))
                    (else (let end-loop
                            ((end end) (offset (arithmetic-shift offset -1)))
                            (define next (+ end offset))
                            (if (= offset 0)
                              (begin (loop (+ 1 bi) i (+ 1 end) out vs)
                                     (d-loop (+ 1 end)))
                              (end-loop
                                (cond ((and (< next initial-big-pos)
                                            (= d (dref (unsafe-vector*-ref
                                                         out next) bi)))
                                       next)
                                      (else end))
                                (arithmetic-shift offset -1))))))))))))
  out)

(define (nlist-intersection nlists)
  (if (null? nlists) '()
    (let loop ((i** nlists))
      (cond ((ormap null? i**) '())
            (else (define i0* (map car i**))
                  (define next (apply max i0*))
                  (if (andmap (lambda (i) (= i next)) i0*)
                    (cons next (loop (map cdr i**)))
                    (loop (map (lambda (i*) (dropf i* (lambda (i) (< i next))))
                               i**))))))))

(define i0              (char->integer #\0))
(define i9              (char->integer #\9))
(define iA              (char->integer #\A))
(define iZ              (char->integer #\Z))
(define (searchable? c) (and (<= i0 c) (<= c iZ) (or (<= c i9) (<= iA c))))
(define (string/searchable s)
  (define cs (map char->integer (string->list (string-upcase s))))
  (list->string (map integer->char (filter searchable? cs))))

;; vector-sparse-find: find the corpus entry with file offset of exactly foffs.
;; Requires corpus to be ordered as they are read from primary storage (by increasing foffs).
(define (vector-sparse-find corpus foffs)
  (unless (integer? foffs) (error "expected integer foffs"))
  (define (iter j-min j-max)
    (if (<= (- j-max j-min) 1)
        (begin
          ;(printf "return j-min=~a j-max=~a\n" j-min j-max)
          (car (vector-ref corpus j-min)))
        (let* ((j-avg (floor (/ (+ j-min j-max) 2)))
               (s-foffs-avg (vector-ref corpus j-avg))
               (foffs-avg (cdr s-foffs-avg)))
          (if (<= foffs-avg foffs)
              (begin
                ;(printf "iter right j-min=~a j-max=~a foffs=~a j-avg=~a s-foffs-avg=~a\n" j-min j-max foffs j-avg s-foffs-avg)
                (iter j-avg j-max))
              (begin
                ;(printf "iter left j-min=~a j-max=~a foffs=~a j-avg=~a s-foffs-avg=~a\n" j-min j-max foffs j-avg s-foffs-avg)
                (iter j-min j-avg))))))
  (iter 0 (vector-length corpus)))

(define (suffix1->string corpus s)
  (substring (vector-ref corpus (car s)) (cdr s)))
(define (suffix2->string corpus s)
  (substring (vector-sparse-find corpus (car s)) (cdr s)))
(define (suffix-string-length1 corpus s)
  (- (string-length (vector-ref corpus (car s))) (cdr s)))
(define (suffix-string-length2 corpus s)
  (- (string-length (vector-sparse-find corpus (car s))) (cdr s)))
(define (suffix-string-ref1 corpus s i)
  (char->integer (string-ref (unsafe-vector*-ref corpus (car s))
                             (+ (cdr s) i))))
(define (suffix-string-ref2 corpus s i)
  (char->integer (string-ref (vector-sparse-find corpus (car s))
                             (+ (cdr s) i))))
(define (string<?/suffixes a ai b bi)
  (define alen (- (string-length a) ai))
  (define blen (- (string-length b) bi))
  (let loop ((k (min alen blen)) (ai ai) (bi bi))
    (cond ((= k 0)                                      (< alen blen))
          ((char<? (string-ref a ai) (string-ref b bi)) #t)
          ((char>? (string-ref a ai) (string-ref b bi)) #f)
          (else (loop (- k 1) (+ ai 1) (+ bi 1))))))
(define (suffix<?/corpus1 corpus a b)
  (string<?/suffixes (vector-ref corpus (car a)) (cdr a)
                     (vector-ref corpus (car b)) (cdr b)))
(define (suffix<?/corpus2a corpus a b)
  (string<? (substring (vector-sparse-find corpus (car a)) (cdr a))
            (substring (vector-sparse-find corpus (car b)) (cdr b))))
(define (suffix<?/corpus2 hashcorpus a b)
  (let* ((c (hash-ref hashcorpus (car a)))
         (d (hash-ref hashcorpus (car b))))
    (string<?/suffixes c (cdr a) d (cdr b))))

(define (suffix:corpus->index corpus)
  (printf "corpus[0]=~a\n" (vector-ref corpus 0))
  (define-values
    (suffix-string-length suffix<?/corpus suffix-string-ref)
    (if (pair? (vector-ref corpus 0))
      (values suffix-string-length2 suffix<?/corpus2 suffix-string-ref2)
      (values suffix-string-length1 suffix<?/corpus1 suffix-string-ref1)))
  (define suffixes
    (foldl (lambda (i all)
             (foldl (lambda (j all) (cons (cons i j) all))
                    all (range (string-length (vector-ref corpus i)))))
           '() (range (vector-length corpus))))
  (define (suffix<? a b)    (suffix<?/corpus corpus a b))
  (define (suffix-length s) (suffix-string-length corpus s))
  (define (suffix-ref s i)  (suffix-string-ref corpus s i))
  (msd-radix-sort suffixes suffix-length suffix-ref suffix<?))

(define (remove-adjacent-duplicates xs)
  (define (remove/x x xs)
    (cons x (let loop ((xs xs))
              (cond ((null? xs)              '())
                    ((equal? (car xs) x)     (loop (cdr xs)))
                    (else (remove/x (car xs) (cdr xs)))))))
  (if (null? xs) '() (remove/x (car xs) (cdr xs))))

(define (dedup/< ns) (remove-adjacent-duplicates (sort ns <)))

(define (suffix:corpus-find-range corpus index str)
  (define suffix->string
    (if (pair? (vector-ref corpus 0))
      suffix2->string
      suffix1->string))
  (define needle (string/searchable str))
  (define (compare si needle)
    (define hay (suffix->string corpus (suffix-key-ref index si)))
    (cond ((string-prefix? hay needle) 0)
          ((string<? hay needle)      -1)
          (else                        1)))
  ;; Find a point in the desired range...
  (let find-range ((start 0) (end (suffix-key-count index)))
    (cond ((< start end)
           (define mid (+ start (quotient (- end start) 2)))
           (case (compare mid needle)
             ((-1) (find-range (+ 1 mid) end))
             (( 1) (find-range start mid))
             (( 0) ;; ... then find the start and end of that range.
              (define rstart
                (let loop ((start start) (end mid))
                  (cond ((< start end)
                         (define mid (+ start (quotient (- end start) 2)))
                         (case (compare mid needle)
                           ((-1) (loop (+ 1 mid) end))
                           (( 0) (loop start mid))
                           (else (error "rstart: this shouldn't happen."))))
                        (else end))))
              (define rend
                (let loop ((start (+ 1 mid)) (end end))
                  (cond ((< start end)
                         (define mid (+ start (quotient (- end start) 2)))
                         (case (compare mid needle)
                           ((1) (loop start mid))
                           ((0) (loop (+ 1 mid) end))
                           (else (error "rend: this shouldn't happen."))))
                        (else end))))
              (cons rstart rend))))
          (else (cons start end)))))

(define (suffix:corpus-find* corpus index str*)
  (define (rz r) (- (cdr r) (car r)))
  (define rs (map (lambda (s) (suffix:corpus-find-range corpus index s)) str*))
  (define zmin (* 2 (if (null? rs) 0 (foldl (lambda (r z) (min z (rz r)))
                                            (rz (car rs)) (cdr rs)))))
  (nlist-intersection
    (map (lambda (r) (dedup/< (map (lambda (i) (car (suffix-key-ref index i)))
                                   (range (car r) (cdr r)))))
         (filter (lambda (r) (<= (rz r) zmin)) rs))))

(define (suffix:corpus-find-range/disk cid->concept in-index str)
  (define needle (string/searchable str))
  (define (compare si needle)
    (match-define (cons cid pos) (suffix-index->suffix-key in-index si))
    (define hay (substring (string/searchable (concept-name (cid->concept cid))) pos))
    (cond ((string-prefix? hay needle) 0)
          ((string<? hay needle)      -1)
          (else                        1)))
  ;; Find a point in the desired range...
  (let find-range ((start 0) (end (suffix-key-count/port in-index)))
    (cond ((< start end)
           (define mid (+ start (quotient (- end start) 2)))
           (case (compare mid needle)
             ((-1) (find-range (+ 1 mid) end))
             (( 1) (find-range start mid))
             (( 0) ;; ... then find the start and end of that range.
              (define rstart
                (let loop ((start start) (end mid))
                  (cond ((< start end)
                         (define mid (+ start (quotient (- end start) 2)))
                         (case (compare mid needle)
                           ((-1) (loop (+ 1 mid) end))
                           (( 0) (loop start mid))
                           (else (error "rstart: this shouldn't happen."))))
                        (else end))))
              (define rend
                (let loop ((start (+ 1 mid)) (end end))
                  (cond ((< start end)
                         (define mid (+ start (quotient (- end start) 2)))
                         (case (compare mid needle)
                           ((1) (loop start mid))
                           ((0) (loop (+ 1 mid) end))
                           (else (error "rend: this shouldn't happen."))))
                        (else end))))
              (cons rstart rend))))
          (else (cons start end)))))

(define (suffix:corpus-find*/disk cid->concept in-index str*)
  (define (rz r) (- (cdr r) (car r)))
  (define rs (map (lambda (s) (suffix:corpus-find-range/disk cid->concept in-index s)) str*))
  (define zmin (* 2 (if (null? rs) 0 (foldl (lambda (r z) (min z (rz r)))
                                            (rz (car rs)) (cdr rs)))))
  (nlist-intersection
    (map (lambda (r) (dedup/< (map (lambda (i) (car (suffix-index->suffix-key in-index i)))
                                   (range (car r) (cdr r)))))
         (filter (lambda (r) (<= (rz r) zmin)) rs))))

(define (string:corpus->index corpus)
  (define ixs (range (vector-length corpus)))
  (define (ix<? a b) (string<? (vector-ref corpus a) (vector-ref corpus b)))
  (define (ix-length ix) (string-length (vector-ref corpus ix)))
  (define (ix-ref ix i)  (char->integer (string-ref (vector-ref corpus ix) i)))
  (msd-radix-sort ixs ix-length ix-ref ix<?))

(define (string:corpus-find corpus index needle)
  (define (compare si needle)
    (define hay (vector-sparse-find corpus (vector-ref index si)))
    (cond ((string=? hay needle)  0)
          ((string<? hay needle) -1)
          (else                   1)))
  ;; Find a point in the desired range...
  (let find-range ((start 0) (end (vector-length index)))
    (cond ((< start end)
           (define mid (+ start (quotient (- end start) 2)))
           (case (compare mid needle)
             ((-1) (find-range (+ 1 mid) end))
             (( 1) (find-range start mid))
             (( 0) ;; ... then find the start and end of that range.
              (define rstart
                (let loop ((start start) (end mid))
                  (cond ((< start end)
                         (define mid (+ start (quotient (- end start) 2)))
                         (case (compare mid needle)
                           ((-1) (loop (+ 1 mid) end))
                           (( 0) (loop start mid))
                           (else (error "rstart: this shouldn't happen."))))
                        (else end))))
              (define rend
                (let loop ((start (+ 1 mid)) (end end))
                  (cond ((< start end)
                         (define mid (+ start (quotient (- end start) 2)))
                         (case (compare mid needle)
                           ((1) (loop start mid))
                           ((0) (loop (+ 1 mid) end))
                           (else (error "rend: this shouldn't happen."))))
                        (else end))))
              (remove-duplicates (map (lambda (i) (vector-ref index i))
                                      (range rstart rend))))))
          (else '()))))

(define (string:corpus-find* corpus index str*)
  (remove-duplicates
    (sort (append* (map (lambda (s) (string:corpus-find corpus index s)) str*))
          <)))

(define (string:corpus-find/disk cid->concept in-index needle)
  (define (compare si needle)
    (define cid (string-index->string-key in-index si))
    (define hay (concept-cui (cid->concept cid)))
    (cond ((string=? hay needle)  0)
          ((string<? hay needle) -1)
          (else                   1)))
  ;; Find a point in the desired range...
  (let find-range ((start 0) (end (string-key-count in-index)))
    (cond ((< start end)
           (define mid (+ start (quotient (- end start) 2)))
           (case (compare mid needle)
             ((-1) (find-range (+ 1 mid) end))
             (( 1) (find-range start mid))
             (( 0) ;; ... then find the start and end of that range.
              (define rstart
                (let loop ((start start) (end mid))
                  (cond ((< start end)
                         (define mid (+ start (quotient (- end start) 2)))
                         (case (compare mid needle)
                           ((-1) (loop (+ 1 mid) end))
                           (( 0) (loop start mid))
                           (else (error "rstart: this shouldn't happen."))))
                        (else end))))
              (define rend
                (let loop ((start (+ 1 mid)) (end end))
                  (cond ((< start end)
                         (define mid (+ start (quotient (- end start) 2)))
                         (case (compare mid needle)
                           ((1) (loop start mid))
                           ((0) (loop (+ 1 mid) end))
                           (else (error "rend: this shouldn't happen."))))
                        (else end))))
              (remove-duplicates (map (lambda (i) (string-index->string-key in-index i))
                                      (range rstart rend))))))
          (else '()))))

(define (string:corpus-find*/disk cid->concept in-index str*)
  (remove-duplicates
    (sort (append* (map (lambda (s) (string:corpus-find/disk cid->concept in-index s)) str*))
          <)))

;;; param-fd-input-binary:
;;;   A context for caching file descriptors.
;;;   To preserve context, (make-hash), keep a reference, and pass
;;;   via parameterize.
(define param-fd-input-binary
  (make-parameter
    'initialize-me-via-make-hash
    #f))

;;; ensure-fd-input-binary:
;;;   Provide the cached file handle for fn, or open a file handle
;;;   if one hasn't been created.
(define (ensure-fd-input-binary absf)
  (define (ensure-fd h)
    (dict-ref! h absf (lambda () (open-input-file absf #:mode 'binary))))
  (define h (param-fd-input-binary))
  (if (dict? h)
    (ensure-fd h)
    (let* (
        (h (make-hash))
        (fd (ensure-fd h)))
      (param-fd-input-binary h)
      fd)))

(define (test:parameterize-defaults thunk)
  (parameterize
    ((param-fd-input-binary (make-hash)))
    (thunk)))
