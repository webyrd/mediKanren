#lang racket/base
(provide
  string/searchable
  suffix:corpus->index
  suffix:corpus-find*
  string:corpus->index
  string:corpus-find*)
(require
  racket/list
  racket/string
  racket/vector)

(define (nlist-intersection nlists)
  (let loop ((i** (map (lambda (nlist) (sort nlist <)) nlists)))
    (cond ((ormap null? i**) '())
          (else (define i0* (map car i**))
                (define next (apply max i0*))
                (if (andmap (lambda (i) (= i next)) i0*)
                  (cons next (loop (map cdr i**)))
                  (loop (map (lambda (i*) (dropf i* (lambda (i) (< i next))))
                             i**)))))))

(define i0              (char->integer #\0))
(define i9              (char->integer #\9))
(define iA              (char->integer #\A))
(define iZ              (char->integer #\Z))
(define (searchable? c) (and (<= i0 c) (<= c iZ) (or (<= c i9) (<= iA c))))
(define (string/searchable s)
  (define cs (map char->integer (string->list (string-upcase s))))
  (list->string (map integer->char (filter searchable? cs))))
(define (suffix->string corpus s)
  (substring (vector-ref corpus (car s)) (cdr s)))

(define (suffix:corpus->index corpus)
  (define suffixes
    (foldl (lambda (i all)
             (foldl (lambda (j all) (cons (cons i j) all))
                    all (range (string-length (vector-ref corpus i)))))
           '() (range (vector-length corpus))))
  (define (suffix<? a b) (string<? (suffix->string corpus a)
                                   (suffix->string corpus b)))
  (vector-sort (list->vector suffixes) suffix<?))

(define (suffix:corpus-find corpus index str)
  (define needle (string/searchable str))
  (define (compare si needle)
    (define hay (suffix->string corpus (vector-ref index si)))
    (cond ((string-prefix? hay needle) 0)
          ((string<? hay needle)      -1)
          (else                        1)))
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
              (remove-duplicates (map (lambda (i) (car (vector-ref index i)))
                                      (range rstart rend))))))
          (else '()))))

(define (suffix:corpus-find* corpus index str*)
  (nlist-intersection
    (map (lambda (s) (suffix:corpus-find corpus index s)) str*)))

(define (string:corpus->index corpus)
  (define ixs (range (vector-length corpus)))
  (define (ix<? a b) (string<? (vector-ref corpus a) (vector-ref corpus b)))
  (vector-sort (list->vector ixs) ix<?))

(define (string:corpus-find corpus index needle)
  (define (compare si needle)
    (define hay (vector-ref corpus (vector-ref index si)))
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
