#lang racket/base
(provide (all-from-out "common.rkt") (all-from-out "syntax.rkt")
         (all-from-out "constraint.rkt")
         run^ run run*)
(require "common.rkt" "constraint.rkt" "stream.rkt" "syntax.rkt")

;(define query->stream dfs:query->stream)
(define query->stream bis:query->stream)

(define-syntax run^
  (syntax-rules () ((_   body ...) (query->stream (query  body ...)))))
(define-syntax run
  (syntax-rules () ((_ n body ...) (s-take n      (run^   body ...)))))
(define-syntax run*
  (syntax-rules () ((_   body ...)                (run #f body ...))))
