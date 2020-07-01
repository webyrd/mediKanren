#lang racket
(require
 "../db.rkt"
 "query.rkt")
(require racket/set)

;; working in progress

(define edges (run 1000 (e) (edgeo e)))

(define (triple e)
  (list
   (cdr (concept->category (edge->subject e)))
   (cdr (edge->pred e))
   (cdr (concept->category (edge->object e)))))

(define results (mutable-set))

(define (add-triple! e)
  (set-add! results (triple e)))

(for-each (lambda (e) (add-triple! e)) edges)

(define out (open-output-file "out.dot" #:exists 'replace))
(fprintf out "digraph{\n")
(set-for-each
 results
 (lambda (t)
   (apply fprintf out "~s -> ~s -> ~s\n" t)))
(fprintf out "}\n")
(close-output-port out)
