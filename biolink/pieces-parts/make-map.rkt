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
           (predicate (cdr (edge->pred e)))
           (object-prefix (concept->prefix (edge->object e))))
       (set-add s (list subject-prefix object-prefix predicate))))
   (set) result))


(define out (open-output-file "orange_graph.dot" #:exists 'replace))
(fprintf out "digraph{\ngraph [ bgcolor=lightgray, fontname=Arial, fontcolor=blue, 
                         fontsize=12 ];\n
                 node [ fontname=Arial, fontcolor=blue, fontsize=11];\n
                 edge [ fontname=Helvetica, fontcolor=red, fontsize=10, labeldistance=2, labelangle=-50 ];\n
    splines=\"FALSE\";\n
    rankdir=\"LR\";\n")
(set-for-each
 graph
 (lambda (t)
   (apply fprintf out "\t~s -> ~s [label=~s]\n" t)))
(fprintf out "}\n")
(close-output-port out)


