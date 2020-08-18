#lang racket
(require
  "../repr.rkt"
  "../common.rkt"
  )

(define db (cdr (car (databases))))

(define result (time (run* (e) (edgeo e))))


(define (concept->prefix concept)
  (car (string-split (concept->curie concept) ":" #:trim? #f))
  )

(define graph
  (foldl
   (lambda (e s)
     (let ((subject-prefix (concept->prefix (edge->subject e)))
           (object-prefix (concept->prefix (edge->object e)))
           (predicate (cdr (edge->pred e))))
       (set-add s (list subject-prefix predicate object-prefix))))
   (set) result))



