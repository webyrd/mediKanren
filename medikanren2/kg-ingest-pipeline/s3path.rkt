#lang racket
(provide
    (all-defined-out))

(define (s3split-impl path)
    (string-split path "/" #:trim? #f))

(define (s3split-from-uri path-base path)
    (list-tail (s3split-impl path) (+ 1 (length (s3split-impl path-base)))))
