#lang racket/base
(provide
  f:query f:true f:false f:relate f:imply f:iff f:or f:and f:not f:exist f:all
  t:query t:quote t:var t:prim t:app t:lambda t:if t:let
  scm->term)
(require "misc.rkt" (except-in racket/match ==) racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-variant formula?
  (f:query  result param f)  ; TODO: omit this class of formula?
  (f:true)
  (f:false)
  ;; TODO: local relation definitions, to express subqueries
  ;; but this may be macro-expressible without a dedicated AST node?
  ;(f:letrec defs)  ; defs ::= list-of (name params f)
  (f:relate relation args)  ; finite position built-in? #f for infinite relations
  ;; TODO:
  ;(f:relate relation arg)  ; switch to single argument for apply-relation, where variadic version is sugar

  ;(f:meta thunk)

  ;; or maybe just aggregate via forall/not-exists/any<=o enumeration
  (f:imply  if then)
  (f:iff    f1 f2)
  (f:or     f1 f2)
  (f:and    f1 f2)
  (f:not    f)

  ;; TODO: single-param versions of f:exist and f:all:
  ;(f:exist  param body)
  ;(f:all    param body)

  (f:exist  params body)
  (f:all    params body))

(define-variant term?
  ;; TODO: simplify, removing t:query, t:lambda, t:if, t:let, t:prim, t:app
  ;; replace t:app with t:cons and t:vector
  (t:query  name formula)
  (t:quote  value)
  (t:var    name)
  (t:prim   name)
  (t:app    proc args)
  (t:lambda params body)
  (t:if     c t f)
  (t:let    bpairs body))

(define (t:cons         a d) (t:app (t:prim 'cons)          (list a d)))
(define (t:list->vector xs)  (t:app (t:prim 'list->vector)  (list xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Values and term conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (atom? x)
  (or (null? x) (boolean? x) (symbol? x) (string? x) (bytes? x) (and (real? x) (exact? x)) (void? x)))

(define (scm->term x)
  (cond ((term?         x)  x)
        ((pair?         x)  (t:cons         (scm->term (car          x))
                                            (scm->term (cdr          x))))
        ((vector?       x)  (t:list->vector (scm->term (vector->list x))))
        ((atom?         x)  (t:quote        x))
        ((and (real?    x)
              (inexact? x)) (scm->term (inexact->exact x)))
        (else               (error "invalid dbk value:" x))))

(define (f-relations f)
  (match f
    ((f:query   _ _ f)       (f-relations f))
    ;; TODO: no need for t-relations* if we lift all uses of t:query
    ;((f:relate  relation args) (set-add (t-relations* args) relation))
    ((f:relate  relation _)  (set relation))
    ((f:imply   f1 f2)       (set-union (f-relations f1) (f-relations f2)))
    ((f:iff     f1 f2)       (set-union (f-relations f1) (f-relations f2)))
    ((f:or      f1 f2)       (set-union (f-relations f1) (f-relations f2)))
    ((f:and     f1 f2)       (set-union (f-relations f1) (f-relations f2)))
    ((f:not     f)           (f-relations f))
    ((f:exist   _ body)      (f-relations body))
    ((f:all     _ body)      (f-relations body))
    ((or (f:true) (f:false)) (set))))
