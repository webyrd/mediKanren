#lang racket
(provide (all-defined-out)
         (all-from-out "../../common.rkt"))
(require "../../common.rkt")

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

(define S (keep 1 (find-concepts #f (list "imatin"))))
;; Alternatively, fuzzy search by name.
;(define O (keep 1 (find-concepts #f (list "asthma"))))
(define O (keep 1 (find-concepts #t (list "UMLS:C0004096"
                                          "DOID:2841"
                                          "HP:0002099"
                                          "MONDO:0004979"))))

(define gene (find-categories (list "gene")))

(match-define
  (list name=>concepts name=>edges)
  (time
    (run/graph
      ;; Introduce concept names.  Optionally constrain them with lists of
      ;; either concepts or categories.
      ((X gene)  ;; To ask for all Xs instead, specify this as (X #f).
       (S S)
       (O O))

      ;; Introduce edge names.  Optionally constrain them with predicate lists.
      ((S->X decreases)
       (X->O increases))

      ;; Specify one or more directed paths.  Paths imply edges with subject on
      ;; the left and object on the right.
      (S S->X X X->O O)
      ;; This path could also have been equivalently specified as two separate
      ;; paths (in either order), each containing a single edge:
      ;; (X X->O O)
      ;; (S S->X X)
      )))

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

  (define S (keep 1 (find-concepts #f (list "imatin"))))
  (define O (keep 1 (find-concepts #f (list "asthma"))))

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
