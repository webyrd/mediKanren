#lang racket
(require "common.rkt")

(define (decreases? p)
  (member (cddr p) '("negatively_regulates"
                     "prevents"
                     "treats"
                     "disrupts"
                     "increases_degradation_of"
                     "decreases_activity_of"
                     "decreases_expression_of")))
(define (increases? p)
  (member (cddr p) '("positively_regulates"
                     "produces"
                     "causes"
                     "causes_condition"
                     "causally_related_to"
                     "contributes_to"
                     "gene_associated_with_condition"
                     "gene_mutations_contribute_to"
                     "decreases_degradation_of"
                     "increases_activity_of"
                     "increases_expression_of")))

(define S (take (find-concepts #t #f 0 #f  (list "imatin")) 1))
(define O (take (find-concepts #f #t 0 #f  (list "asthma")) 1))

(define SP (map (lambda (cp)
                  (define c (car cp))
                  (define pS (cadr cp))
                  (list c (filter decreases? pS) #f))
                (find-predicates/concepts #t #f S)))
(define OP (map (lambda (cp)
                  (define c (car cp))
                  (define pO (caddr cp))
                  (list c #f (filter increases? pO)))
                (find-predicates/concepts #f #t O)))

(define X (find-Xs SP OP))

(define x-bcr (car (filter (lambda (x) (equal? (cadddr (car x)) "BCR gene")) X)))

(displayln 'X:)
(pretty-print (car x-bcr))
(newline)
(displayln 'S-edges:)
(pretty-print (cadr x-bcr))
(newline)
(displayln 'O-edges:)
(pretty-print (caddr x-bcr))
