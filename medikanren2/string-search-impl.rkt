#lang racket/base
(provide
  string/searchable
  suffix:corpus-find*/disk
  (prefix-out test: string/searchable)
  (prefix-out test: suffix-key-count/port)
  (prefix-out test: suffix-index->suffix-key)
  (prefix-out test:  suffix-key-count/port)
  (prefix-out test:  suffix-index->suffix-key)
  bytes->suffix-key
  suffix-key->bytes
  (prefix-out test: bytes->string-key)
  (prefix-out test: string-key->bytes)
  param-fd-input-binary
  ensure-fd-input-binary
  test:parameterize-defaults

  ;; The following are internals used in string-search.rkt
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

(define (string<?/suffixes a ai b bi)
  (define alen (- (string-length a) ai))
  (define blen (- (string-length b) bi))
  (let loop ((k (min alen blen)) (ai ai) (bi bi))
    (cond ((= k 0)                                      (< alen blen))
          ((char<? (string-ref a ai) (string-ref b bi)) #t)
          ((char>? (string-ref a ai) (string-ref b bi)) #f)
          (else (loop (- k 1) (+ ai 1) (+ bi 1))))))
(define (suffix<?/corpus2 hashcorpus bin-a bin-b)
  (let* ((a (bytes->suffix-key bin-a 0))
         (b (bytes->suffix-key bin-b 0))
         (c (hash-ref hashcorpus (car a)))
         (d (hash-ref hashcorpus (car b))))
    (string<?/suffixes c (cdr a) d (cdr b))))

(define (remove-adjacent-duplicates xs)
  (define (remove/x x xs)
    (cons x (let loop ((xs xs))
              (cond ((null? xs)              '())
                    ((equal? (car xs) x)     (loop (cdr xs)))
                    (else (remove/x (car xs) (cdr xs)))))))
  (if (null? xs) '() (remove/x (car xs) (cdr xs))))

(define (dedup/< ns) (remove-adjacent-duplicates (sort ns <)))

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
