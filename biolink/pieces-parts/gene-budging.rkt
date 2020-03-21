#lang racket
(provide (all-defined-out))
(require "query.rkt")


(define make-directly-regulate-gene
  (lambda (regulation-predicates)
    (lambda (gene-curie)
      (displayln "\nRunning 1-hop up query with concept categories and drug safety")
      (define q (time (query/graph
                       (
                        ;; concepts
                        (X       drug)
                        (my-gene gene-curie)
                        (T       #f))
                       ;; edges
                       ((X->my-gene regulation-predicates)
                        (X->T       drug-safe))
                       ;; paths
                       (X X->my-gene my-gene)
                       (X X->T T))))
      q)))

(define directly-upregulate-gene (make-directly-regulate-gene positively-regulates))
(define directly-downregulate-gene (make-directly-regulate-gene negatively-regulates))



(define curie-to-anything
  (lambda (curie predicate*)
    (query/graph
      (;; concepts
       (X curie)
       (T #f))
      ;; edges
      ((X->T predicate*))
      ;; paths      
      (X X->T T))))

(define curie-to-tradenames
  (lambda (curie)
    (curie-to-anything curie '("has_tradename"))))

(define curie-to-clinical-trials
  (lambda (curie)
    (curie-to-anything curie '("clinically_tested_approved_unknown_phase"
                               "clinically_tested_terminated_phase_2"
                               "clinically_tested_terminated_phase_3"
                               "clinically_tested_terminated_phase_2_or_phase_3"
                               "clinically_tested_withdrawn_phase_3"
                               "clinically_tested_withdrawn_phase_2_or_phase_3"
                               "clinically_tested_withdrawn_phase_2"
                               "clinically_tested_suspended_phase_2"
                               "clinically_tested_suspended_phase_3"
                               "clinically_tested_suspended_phase_2_or_phase_3"))))

(define curie-to-indicated_for
  (lambda (curie)
    (curie-to-anything curie '("indicated_for"))))

(define curie-to-contraindicated_for
  (lambda (curie)
    (curie-to-anything curie '("contraindicated_for"))))


(define pubmed-URLs-from-bogo-edge
  (lambda (bogo-edge)
    (define concrete-edges (list-ref bogo-edge 2))
    (remove-duplicates
      (append*
        (map pubmed-URLs-from-edge concrete-edges)))))


(define drug-info-for-curie
  (lambda (curie)
    (map
     (lambda (l)
       (match l
         [`(,name . ,a)
          (cons name (map curie-synonyms/names (curies/query a 'T)))]))
     (list 
      (cons 'tradenames (curie-to-tradenames curie))
      (cons 'clinical-trials (curie-to-clinical-trials curie))
      (cons 'indicated_for (curie-to-indicated_for curie))
      (cons 'contraindicated_for (curie-to-contraindicated_for curie))))))


(define drug-info-from-bogo-edge
  (lambda (bogo-edge)
    (define curie (caar bogo-edge))
    (define pubmed-URLs (pubmed-URLs-from-bogo-edge bogo-edge))
    (append
     (list (cons 'curie curie))
     (list (cons 'curie-synonyms/names (curie-synonyms/names curie)))
     (drug-info-for-curie curie)
     (list (cons 'pubmeds pubmed-URLs)))))





;; kdm1a
(define kdm1a-directly-up (directly-upregulate-gene "HGNC:29079"))
;; returns the set of all query results (for X, for gene, for edges X->my-gene, etc.)

(define kdm1a-directly-up-Xs (curies/query kdm1a-directly-up 'X))

;; each edge corresponds to an X in kdm1a-Xs
(define edges/X->kdm1a-directly-up (edges/ranked (ranked-paths kdm1a-directly-up) 0 0))

(define kdm1a-directly-up-drug-info (map drug-info-from-bogo-edge edges/X->kdm1a-directly-up))


#|
(define kdm1a-directly-down (directly-downregulate-gene "HGNC:29079"))
;; returns the set of all query results (for X, for gene, for edges X->my-gene, etc.)

(define kdm1a-directly-down-Xs (curies/query kdm1a-directly-down 'X))

;; each edge corresponds to an X in kdm1a-Xs
(define edges/X->kdm1a-directly-down (edges/ranked (ranked-paths kdm1a-directly-down) 0 0))

;; Does this diverge??? Or is it just slow???
(define kdm1a-directly-down-drug-info (map drug-info-from-bogo-edge edges/X->kdm1a-directly-down))
|#
