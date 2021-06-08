#lang racket/base
(provide (all-from-out "common.rkt") (all-from-out "syntax.rkt")
         (all-from-out "constraint.rkt")
         query->stream
         run^ run run* run/steps run*/steps
         run/set run*/set run/set/steps run*/set/steps)
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
(define-syntax run/steps
  (syntax-rules () ((_ steps n body ...) (s-take/steps steps n (run^               body ...)))))
(define-syntax run*/steps
  (syntax-rules () ((_ steps   body ...)                       (run/steps steps #f body ...))))

(define-syntax run/set
  (syntax-rules () ((_ n body ...) (s-take/set n (run^       body ...)))))
(define-syntax run*/set
  (syntax-rules () ((_   body ...)               (run/set #f body ...))))
(define-syntax run/set/steps
  (syntax-rules () ((_ steps n body ...) (s-take/set/steps steps n (run^                   body ...)))))
(define-syntax run*/set/steps
  (syntax-rules () ((_ steps   body ...)                           (run/set/steps steps #f body ...))))

;; TODO: special case aggregation operators that could be implemented more
;; efficiently than post-processing `run*` results:
;; * run/min, run/max, run/count
