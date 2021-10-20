#lang racket/base
(require racket/list racket/pretty racket/unsafe/ops racket/vector)

;; TODO: pre-allocate a buffer for all the threshold merge sorts to share
;; TODO: try MSD-radix-sort with a threshold for switching to q3sort! ?
;; TODO: try burst/trie sort?

;; multi-key (aka 3-way) quicksort
(define (q3sort! max-depth? x&depth->cmp <? v start end)
  ;(define threshold 4096)
  ;(define threshold 1024)
  (define threshold 128)
  ;(define threshold 64)
  (define (q3partition start end cmp k!)
    (define (cmp/i i) (cmp (unsafe-vector*-ref v i)))
    ;; TODO: are the initial forward/backward scans worth the extra complexity?
    (let loop ((i.low start))
      (if (and (unsafe-fx< i.low end) (unsafe-fx= (cmp/i i.low) -1))
        (loop (unsafe-fx+ i.low 1))
        (let loop ((i.high end))
          ;; (< i.low i.high) is guaranteed since we always have a partitioning element
          (let ((i.high.next (unsafe-fx- i.high 1)))
            (if (unsafe-fx= (cmp/i i.high.next) 1)
              (loop i.high.next)
              (let loop ((i.low i.low) (i.mid i.low) (i.high i.high))
                (if (unsafe-fx< i.mid i.high)
                  (let ((x (unsafe-vector*-ref v i.mid)))
                    (case (cmp x)
                      ((-1) (let ((x.next (unsafe-vector*-ref v i.low)))
                              (unsafe-vector*-set! v i.low x)
                              (unsafe-vector*-set! v i.mid x.next)
                              (loop (unsafe-fx+ i.low 1) (unsafe-fx+ i.mid 1) i.high)))
                      (( 0) (loop i.low (unsafe-fx+ i.mid 1) i.high))
                      (( 1) (let* ((i.high.next (unsafe-fx- i.high 1))
                                   (x.next      (unsafe-vector*-ref v i.high.next)))
                              (unsafe-vector*-set! v i.high.next x)
                              (unsafe-vector*-set! v i.mid       x.next)
                              (loop i.low i.mid i.high.next)))))
                  (k! i.low i.mid)))))))))
  (let loop.next-depth ((depth 0) (start start) (end end))
    (let loop.skip-max ((i start) (start start))
      (if (unsafe-fx< i end)
        (let ((x (unsafe-vector*-ref v i)))
          (cond ((max-depth? x depth) (unsafe-vector*-set! v i     (unsafe-vector*-ref v start))
                                      (unsafe-vector*-set! v start x)
                                      (loop.skip-max (unsafe-fx+ i 1) (unsafe-fx+ start 1)))
                (else                 (loop.skip-max (unsafe-fx+ i 1)             start))))
        (let loop.current-depth ((start start) (end end))
          (if (unsafe-fx<= (unsafe-fx- end start) threshold)
            (vector-sort! v <? start end)
            (q3partition start end (x&depth->cmp (unsafe-vector*-ref v (random start end))
                                                 depth)
                         (lambda (i.low i.mid)
                           (loop.current-depth                   start i.low)
                           (loop.current-depth                   i.mid end)
                           (loop.next-depth (unsafe-fx+ depth 1) i.low i.mid)))))))))

(define (vector-bytes-sort! v (start 0) (end (vector-length v)))
  (define (ref x d) (unsafe-bytes-ref x d))
  (q3sort!
    (lambda (x depth) (unsafe-fx<= (unsafe-bytes-length x) depth))
    (lambda (x depth) (let ((n (bytes-ref x depth)))
                        (lambda (y)
                          (let ((m (bytes-ref y depth)))
                            (cond ((unsafe-fx< m n) -1)
                                  ((unsafe-fx= m n)  0)
                                  (else              1))))))
    bytes<?
    v start end))

(define (vector-string-sort! v (start 0) (end (vector-length v)))
  (define (ref x d) (string-ref x d))
  (q3sort!
    (lambda (x depth) (unsafe-fx<= (unsafe-string-length x) depth))
    (lambda (x depth) (let ((n (ref x depth)))
                        (lambda (y)
                          (let ((m (ref y depth)))
                            (cond ((unsafe-char<? m n) -1)
                                  ((unsafe-char=? m n)  0)
                                  (else                 1))))))
    string<?
    v start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-string)
  (list->string (map (lambda (_) (integer->char (random 128))) (range (random 200)))))

(define (string-sort-and-verify v)
  (define v.expected (time (vector-sort v string<?)))
  (time (vector-string-sort! v))
  (unless (equal? v.expected v)
    (pretty-write 'sort-failed)))

(define (random-bytes)
  (list->bytes (map (lambda (_) (random 128)) (range (random 300 500)))))

(define (bytes-sort-and-verify v)
  ;(define v.expected (time (vector-sort v bytes<?)))
  (define v.expected (time (vector-copy v)))
  (time (vector-sort! v.expected bytes<?))

  (time (vector-bytes-sort! v))
  (unless (equal? v.expected v)
    (pretty-write 'sort-failed)))

;(define test-size 80000000)
;(define test-size 20000000)
(define test-size 5000000)
;(define test-size 500000)

;(string-sort-and-verify (time (list->vector (map (lambda (_) (random-string)) (range test-size)))))
(bytes-sort-and-verify (time (list->vector (map (lambda (_) (random-bytes)) (range test-size)))))
