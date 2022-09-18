#lang racket/base
(provide shared-get shared-put)

(define shared-value 5)

(define (shared-get)
  shared-value)

(define (shared-put x)
  (set! shared-value x))
