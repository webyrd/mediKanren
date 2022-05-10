#lang racket/base
(provide
  ? help
  graph path
  C P T
  show hide count unmodify
  graph? path? C? P? T? show? hide? count? except?
  except
  inote iref aref untag
  )
(require "common.rkt" "mk.rkt")



(define c1 (C (C (C "imatinib") '(db semmed)) '(cui "...")))
;; ((db id cui name ()) ...)

(define c2 (C (C (C "asthma") '(db semmed)) '(cui "...")))
;; ((db id cui name ()) ...)

(define p1 (P (subject 'Concept1 c1) "decreases"))
;; #(subject Concept1
;;   ((db id name) ...)
;;   ((db id cui name ()) ...))

(define p2 (P (object 'Concept2 c2) "increases"))
;; #(object Concept2
;;   ((db id name) ...)
;;   ((db id cui name ()) ...))

(define X (C p1 p2))
;; ((db id cui name ((subject ((predicate . ((concept . edge-props) ...)) ...))
;;                   (object  ((predicate . ((concept . edge-props) ...)) ...))) ...)

(report (C X "gene"))  ;; edges? paths? pubs? sorting by pubcount or path confidence?
