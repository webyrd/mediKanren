#lang racket/base
(provide
  facts fresh conde
  define-relation
  run*)
(require "micro.rkt")  ; Basic micro core with quadratic time fact accumulation
;(require "micro-with-sets.rkt")  ; A faster version of the micro core

;;; Formulas are wrapped ambitions which discover their producer dependencies.

(define (fm:true  seen d*) (values seen d* unit))
(define (fm:false seen d*) (values seen d* fail))

(define (fm:and fm0 fm1)
  (lambda (seen p*)
    (let-values (((seen p* a0) (fm0 seen p*)))
      (let-values (((seen p* a1) (fm1 seen p*)))
        (values seen p* (conj a0 a1))))))
(define (fm:or fm0 fm1)
  (lambda (seen p*)
    (let-values (((seen p* a0) (fm0 seen p*)))
      (let-values (((seen p* a1) (fm1 seen p*)))
        (values seen p* (disj a0 a1))))))

(define (fm:and+ fm0 fm*)
  (if (null? fm*)
    fm0
    (fm:and fm0 (fm:and+ (car fm*) (cdr fm*)))))
(define (fm:or+ fm0 fm*)
  (if (null? fm*)
    fm0
    (fm:or fm0 (fm:or+ (car fm*) (cdr fm*)))))

(define (fm:and* fm*)
  (if (null? fm*)
    fm:true
    (fm:and+ (car fm*) (cdr fm*))))
(define (fm:or* fm*)
  (if (null? fm*)
    fm:false
    (fm:or+ (car fm*) (cdr fm*))))

(define (fm:== t0 t1) (lambda (seen p*) (values seen p* (== t0 t1))))

;;;;;;;;;;;;;;
;;; Syntax ;;;
;;;;;;;;;;;;;;

;; TODO: defining facts using == leads to enormous fact production redundancy,
;; exposing performance problems with unique-append since it is a quadratic
;; implementation of set union.  Since we want to be able to support large
;; relation definitions, the right way to fix this is for the core to store
;; facts in efficient sets instead of lists.  But we could also reduce
;; redundancy by stratifying the evaluation of relation dependencies.  This
;; would lead to fact-oriented relations only being evaluated once, at the
;; start of a run.
(define (facts vars tuples)
  (fm:or* (map (lambda (tuple) (fm:== vars tuple)) tuples)))

;; TODO: rule safety checking?
(define-syntax-rule
  (define-relation (name attr ...) fm0 fm* ...)
  (define (name attr ...)
    (lambda (seen p*)
      (let ((head   (list name (var 'attr) ...))
            (fm     (fresh (attr ...) fm0 fm* ...))
            (a.call (relate (list name attr ...))))
        (if (member name seen)
          (values seen p* a.call)
          (let-values (((seen p* a) (fm (cons name seen) p*)))
            (values seen (cons (realize head a) p*) a.call)))))))

(define-syntax-rule
  (fresh (x ...) fm0 fm* ...)
  (let ((x (var 'x)) ...) (fm:and+ fm0 (list fm* ...))))

(define-syntax-rule
  (conde (fm00 fm0* ...)
         (fm0 fm* ...) ...)
  (fm:or+ (fm:and+ fm00 (list fm0* ...))
          (list (fm:and+ fm0 (list fm* ...) ...) ...)))

(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) fm0 fm* ...)
     (let ((x (var 'x)) ...)
       (define-relation (query x ...) fm0 fm* ...)
       (let-values (((seen p* a) ((query x ...) '() '())))
         (let ((F* (exhaust* p* '())))
           (map cdr (filter (lambda (F) (eq? (car F) query)) F*))))))
    ((_ x fm0 fm* ...) (map car (run* (x) fm0 fm* ...)))))
