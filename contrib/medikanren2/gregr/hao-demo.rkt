#lang racket/base
(require "dbk.rkt")

;;(define-relation (P x)
;  ;(membero x '(1 2 3 4 5 6 7 8 9 10)))
;;(define-relation (Q x)
;  ;(membero x '(1 3 5 6 8 10)))
;
;(define-relation (P x)
;  (conde ((== x 1))
;         ((== x 2))
;         ((== x 3))
;         ((== x 4))
;         ((== x 5))
;         ((== x 6))
;         ((== x 7))
;         ((== x 8))
;         ((== x 9))
;         ((== x 10))))
;
;(define-relation (Q x)
;  (conde ((== x 1))
;         ((== x 3))
;         ((== x 5))
;         ((== x 6))
;         ((== x 8))
;         ((== x 10))))
;
;(define (frequency P Q)
;  (caar (run* (p)
;          (:== p ()
;               (/ (length (run* (x) (P x) (Q x)))
;                  (length (run* (x) (P x))))))))
;
;;(define (exists P Q) (fresh (x) (P x) (Q x))
;
;;(define (occurs p P Q)
;  ;(:== #t () (<= p (frequency P Q))))
;
;(frequency P Q)
;
;(run* (_) (occurs .4 P Q))
;
;(run* (_) (occurs .8 P Q))
;
;
;(define (forall P Q)
;  (:== #t () (= (length (run* (x) (P x) (Q x)))
;                (length (run* (x) (P x))))))
;
;(run* (_) (forall P Q))
;
;(run* (_) (forall Q P))
;
;
;;; exists
;(run* (x) (P x) (Q x))
;
;(run* (p y z)
;  (fresh (y) (== z y))
;  ;(== y z)
;  (:== p ()
;       (/ (length (run* (x) (P x) (Q x)))
;          (length (run* (x) (P x))))))
;
;(define-relation (R x y)
;  (conde ((== x 1)  (== y 111))
;         ((== x 2)  (== y 111))
;         ((== x 3)  (== y 111))
;         ((== x 4)  (== y 111))
;         ((== x 5)  (== y 111))
;         ((== x 6)  (== y 111))
;         ((== x 7)  (== y 111))
;         ((== x 8)  (== y 111))
;         ((== x 9)  (== y 111))
;         ((== x 10) (== y 111))))
;
;;; forall x . (P x) -> (exists y . (R x y))
;(run* (_)
;  (:== #t () (= (length (run* (x) (P x)
;                          (fresh (y) (R x y))))
;                (length (run* (x) (P x))))))
;
;;; exists y . (forall x . (P x) -> (R x y))
;(run* (y)
;  (fresh (x) (R x y))
;  (:== #t (y) (= (length (run* (x) (P x) (R x y)))
;                 (length (run* (x) (P x))))))

(define-relation (membero x xs)
  (fresh (y ys)
    (== `(,y . ,ys) xs)
    (conde ((==  x y))
           ((membero x ys)))))

(define-syntax exists
  (syntax-rules ()
    ((_ y domain body ...) (fresh (y) domain body ...))))

(define-syntax occurs
  (syntax-rules ()
    ((_ p y domain body ...)
     (let ((P (lambda (y) domain))
           (Q (lambda (y) body ...)))
       (:== #t () (<= p (frequency P Q)))))))

(define-syntax where
  (syntax-rules ()
    ((_ (deps ...) body ...)
     (:== #t (deps ...) (pair? (run 1 (_) body ...))))))

(define-syntax select
  (syntax-rules ()
    ((_ ((v domain) ...) body ...)
     (run* (v ...) (domain v) ... body ...))))

(define-syntax <-
  (syntax-rules ()
    ((_ x subquery body ...)
     (fresh (x xs)
       (:== xs () subquery)
       (membero x xs)
       body ...))))

;; TODO: define all these relations
; timepoint year< exposure-event stressor receptor individual-organism chemical-substance in has-phenotype

(define-relation (exposed-to t y x)
  (fresh (i)
    (exposure-event t i)
    (receptor i y)
    (stressor i x)))

(define-relation (asthma-cohort-organism y)
  (individual-organism y)
  (in y 'asthma-cohort))

(define-relation (timepoint t)
  (conde ((== t 2010))
         ;; TODO: ...
         ((== t 2019))))

(run* (z)
  (<- x (select ((x chemical-substance))
                (where (x) ;(x exposed-to has-phenotype asthma-cohort-organism timepoint)
                  (occurs
                    p y asthma-cohort-organism
                    (exists
                      t1 timepoint
                      (exposed-to t1 y x)
                      (exists
                        t2 timepoint
                        (year< t1 t2)
                        (has-phenotype t2 y 'asthma-exacerbation))))))
      (target x z)))
