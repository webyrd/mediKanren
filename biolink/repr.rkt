#lang racket
(provide
  detail-find
  detail-next
  detail-write
  )

(define (write-scm out scm) (fprintf out "~s\n" scm))

(define offset-size 8)
(define (offset->bytes o) (integer->integer-bytes o offset-size #f #f))
(define (bytes->offset b) (integer-bytes->integer b #f #f 0 offset-size))
(define (offset-write out offset) (write-bytes (offset->bytes offset) out))
(define (offset-count in)
  (file-position in eof)
  (/ (file-position in) offset-size))
(define (offset-ref in n)
  (file-position in (* n offset-size))
  (bytes->offset (read-bytes offset-size in)))

(define (detail-write detail-out offset-out detail)
  (when offset-out (offset-write offset-out (file-position detail-out)))
  (write-scm detail-out detail))
(define (detail-ref detail-in offset-in n)
  (file-position detail-in (offset-ref offset-in n))
  (read detail-in))
(define (detail-next detail-in) (read detail-in))

(define (detail-find detail-in offset-in compare)
  (let loop ((start 0) (end (offset-count offset-in)) (best #f))
    (cond ((< start end)
           (define n (+ start (quotient (- end start) 2)))
           (define detail (detail-ref detail-in offset-in n))
           (case (compare detail)
             ((-1) (loop (+ 1 n) end best))
             ((0) (loop start n n))
             ((1) (loop start n best))))
          (best (detail-ref detail-in offset-in best))
          (else eof))))
