#lang racket/base
;; may require manually installing graphviz, cairo, libjpeg

(require "../../medikanren2/common.rkt" 
         racket/string racket/draw racket/format racket/list
         graph
         (prefix-in pict: pict)
         (prefix-in graphviz: graphviz)
         (prefix-in racket: racket)
         )

(define-relation (subclass-of a b)
  (triple a "biolink:subclass_of" b))

(define (draw-edges edges path)
  (racket:send
   (pict:pict->bitmap
    (graphviz:dot->pict
     (graphviz (directed-graph edges))))
   save-file path 'png))

(define (remove-prefix prefix str)
  (string-replace str prefix ""))

(define (remove-biolink-prefixes edges)
  (let ((rem (lambda (e) (remove-prefix "biolink:" e))))
    (map (lambda (pair)
           (map rem pair))
         edges)))

(define (subtrees all-classes top-level-classes)
  (filter (compose not null?)
          (map (lambda (class)
                 (cons class
                       (let loop ((class class))
                         (let* ((new-edges (filter (lambda (edge) (equal? (cadr edge) class))
                                                   all-classes))
                                (new-nodes (map car new-edges)))
                           (append new-edges (apply append (map loop new-nodes)))))))
               (map car top-level-classes))))

(define (draw-subtrees subtrees)
  (map (lambda (subtree)
         (let ((label (car subtree))
               (graph (cdr subtree)))
           (when (not (null? subtree))
             (draw-edges graph (~a "drawings/" label ".png")))))
       subtrees))

(define all-ontology-classes
  (remove-biolink-prefixes
   (run* (s o)
     (triple s "biolink:subclass_of" o) 
     (is-a s "biolink:OntologyClass")
     (is-a o "biolink:OntologyClass")
     (:== #t (s) (char-upper-case? (car (string->list (remove-prefix "biolink:" s)))))
     (:== #t (o) (char-upper-case? (car (string->list (remove-prefix "biolink:" o)))))
)))

(define top-level-ontology-classes
  (cons '("Annotation")
        (remove-biolink-prefixes
         (run* (s) (subclass-of s "biolink:Entity")))))

;; very strange -- let's hope this changes!
(define all-predicates
  (remove-biolink-prefixes
   (run* (s o)
     (triple s "biolink:subclass_of" o) 
     (is-a s "biolink:OntologyClass")
     (is-a o "biolink:OntologyClass")
     (:== #t (s) (char-lower-case? (car (string->list (remove-prefix "biolink:" s)))))
     (:== #t (o) (char-lower-case? (car (string->list (remove-prefix "biolink:" o)))))
)))

(define top-level-predicates
  (remove-biolink-prefixes
   (run* (s) (subclass-of s "biolink:related_to"))))

(draw-subtrees (subtrees all-ontology-classes top-level-ontology-classes))
(draw-subtrees (subtrees all-predicates '(("related_to"))))
(draw-subtrees (subtrees all-predicates top-level-predicates))
