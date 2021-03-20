#lang racket/base
(provide (all-from-out "common.rkt") (all-from-out "syntax.rkt")
         (all-from-out "constraint.rkt")
         query->stream run^ run run*)
(require "common.rkt" "config.rkt" "constraint.rkt" "stream.rkt" "syntax.rkt"
         (except-in racket/match ==))

(define (query->stream q)
  ((match (or (current-config-ref 'search-strategy) 'biased-interleaving)
     ('biased-interleaving bis:query->stream)
     ('depth-first         dfs:query->stream)
     (strategy (error "invalid search strategy:" strategy)))
   q))

(define-syntax run^
  (syntax-rules () ((_   body ...) (query->stream (query  body ...)))))
(define-syntax run
  (syntax-rules () ((_ n body ...) (s-take n      (run^   body ...)))))
(define-syntax run*
  (syntax-rules () ((_   body ...)                (run #f body ...))))

;; TODO: special case aggregation operators that could be implemented more
;; efficiently than post-processing `run*` results:
;; * run/min, run/max, run/count
