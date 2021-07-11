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
  (f:query  result param f)
  (f:true)
  (f:false)
  (f:relate relation args)
  (f:imply  if then)
  (f:iff    f1 f2)
  (f:or     f1 f2)
  (f:and    f1 f2)
  (f:not    f)
  (f:exist  params body)
  (f:all    params body))

(define-variant term?
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
