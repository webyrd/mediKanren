#lang racket
(provide (all-defined-out)
         (all-from-out "common.rkt" "mk-db.rkt"))
(require "common.rkt" "mk-db.rkt")

(define decreases
  (find-predicates
    '("negatively_regulates"
      "prevents"
      "treats"
      "disrupts"
      "increases_degradation_of"
      "decreases_activity_of"
      "decreases_expression_of")))

(define increases
  (find-predicates
    '("positively_regulates"
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

(define S (keep 1 (find-concepts #t #f 0 #f (list "imatin"))))
(define O (keep 1 (find-concepts #f #t 0 #f (list "asthma"))))

(match-define
  (list name=>concepts name=>edges)
  (run/graph
    ((X #f)
     (S S)
     (O O))

    ((S->X decreases)
     (X->O increases))

    (S S->X X X->O O)))

(displayln "concepts by name:")
(pretty-print name=>concepts)
(newline)
(displayln "edges by name:")
(pretty-print name=>edges)
(newline)

(define (summary name=>xs)
  (hash-map name=>xs (lambda (name xs) (cons name (length xs)))))
(displayln "summary:")
(pretty-print `((concepts: . ,(summary name=>concepts))
                (edges:    . ,(summary name=>edges))))


;; Old testing of find-Xs
(define (test-find-Xs)
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

  (define S (keep 1 (find-concepts #t #f 0 #f  (list "imatin"))))
  (define O (keep 1 (find-concepts #f #t 0 #f  (list "asthma"))))

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
  (pretty-print (caddr x-bcr)))
