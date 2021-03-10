#lang racket/base
(provide
  concept-cui
  concept-name
  concept-type
  )

(define (concept-cui concept) (vector-ref concept 0))
(define (concept-name concept) (vector-ref concept 1))
(define (concept-type concept) (vector-ref concept 2))
