#lang racket
(require
  "../repr.rkt"
  "../common.rkt"
  )

(define db (cdr (car (databases))))

;; gets all edges
(define result (time (run* (e) (edgeo e))))


;; gets the prefix for a certain concept (i.e. "MONDO")
(define (concept->prefix concept)
  (car (string-split (concept->curie concept) ":" #:trim? #f))
  )

;; creates a set of all unique edges ordered by subject, object, and predicate
(define graph
  (foldl
   (lambda (e s)
     (let ((subject-prefix (concept->prefix (edge->subject e)))
           (predicate (cdr (edge->pred e)))
           (object-prefix (concept->prefix (edge->object e))))
       (set-add s (list subject-prefix object-prefix predicate))))
   (set) result))

#|
Outputs the edges into dot format in a file

The entire dot graph can then be copied and pasted into a vis.js file and parsed as a DOT network. 
|#
(define out (open-output-file "orange_graph.dot" #:exists 'replace))
(fprintf out "digraph{graph [ bgcolor=lightgray, fontname=Arial, fontcolor=blue, fontsize=12 ]; node [ fontname=Arial, fontcolor=blue, fontsize=11]; edge [ fontname=Helvetica, fontcolor=red, fontsize=10, labeldistance=2, labelangle=-50 ]; splines=\"FALSE\"; rankdir=\"LR\";")
(set-for-each
 graph
 (lambda (t)
   (apply fprintf out "\t~s -> ~s [label=~s]; " t)))
(fprintf out "}")
(close-output-port out)



