#|
(report/query
 (query/graph
  ((X       "GTPI:8390")
   (T       #f))
  ((X->T       drug-safe))
  (X X->T T)))
|#
;; =>
#|
(running object update: 0 0)
(running edge update: 0)
(running subject update: 1 1)
(running edge update: 0)
(running object update: 0 0)
'((concepts: (X 1) (T 0)) (edges: (X->T 0)))
|#

#|
(define bogus-upregulate-gene
  (lambda (gene-curie)
    (define q (time (query/graph
                     ((X       "GTPI:8390")
                      (my-gene gene-curie)
                      (T       #f))
                     ((X->my-gene positively-regulates)
                      (X->T       drug-safe))
                     (X X->my-gene my-gene)
                     (X X->T T))))
    q))

(define a (bogus-upregulate-gene "HGNC:29079"))
(report/query a)
|#
;;
;; > (report/query a)
;; '((concepts: (X 1) (my-gene 1) (T 0)) (edges: (X->my-gene 1) (X->T 0)))




(define directly-upregulate-gene
  (lambda (gene-curie)
    (displayln "\nRunning 1-hop up query with concept categories and drug safety")
    (define q (time (query/graph
                     ((X       drug)
                      (my-gene gene-curie)
                      (T       #f))
                     ((X->my-gene positively-regulates)
                      (X->T       drug-safe))
                     (X X->my-gene my-gene)
                     (X X->T T))))

    q))

(define curie-to-anything
  (lambda (curie predicate*)
    (query/graph
      ((X curie)
       (T #f))
      ((X->T predicate*))
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

(define kdm1a-Xs (curies/query kdm1a-directly-up 'X))

;; each edge corresponds to an X in kdm1a-Xs
(define edges/X->my-gene (edges/ranked (ranked-paths kdm1a-directly-up) 0 0))

(map drug-info-from-bogo-edge edges/X->my-gene)



#|
;; alms1
(define alms1-directly-up (directly-upregulate-gene "HGNC:428"))
(pretty-ranked (take alms1-directly-up 1))

;; rpgrip1l
(define rpgrip1l-directly-up (directly-upregulate-gene "HGNC:29168"))
(pretty-ranked (take rpgrip1l-directly-up 1))

;; inpp5e
(define inpp5e-directly-up (directly-upregulate-gene "HGNC:21474"))
(pretty-ranked (take inpp5e-directly-up 1))

;; nphp3
(define nphp3-directly-up (directly-upregulate-gene "HGNC:7907"))
(pretty-ranked (take nphp3-directly-up 1))
|#










#|
(define directly-downregulate-gene
  (lambda (gene-curie)
    (displayln "\nRunning 1-hop down query with concept categories and drug safety")
    (define q (time (query/graph
                     ((X       drug)
                      (my-gene gene-curie)
                      (T       #f))
                     ((X->my-gene negatively-regulates)
                      (X->T       drug-safe))
                     (X X->my-gene my-gene)
                     (X X->T T))))

    (displayln "\nRanking paths")
    (define ranked (time (ranked-paths q)))

    ranked))

(define alms1-directly-down (directly-downregulate-gene "HGNC:428"))
(pretty-ranked (take alms1-directly-down 1))

;; rpgrip1l
(define rpgrip1l-directly-down (directly-downregulate-gene "HGNC:29168"))
(pretty-ranked (take rpgrip1l-directly-down 1))

;; inpp5e
(define inpp5e-directly-down (directly-downregulate-gene "HGNC:21474"))
(pretty-ranked (take inpp5e-directly-down 1))

;; nphp3
(define nphp3-directly-down (directly-downregulate-gene "HGNC:7907"))
(pretty-ranked (take nphp3-directly-down 1))

;; kdm1a
(define kdm1a-directly-down (directly-downregulate-gene "HGNC:29079"))
(pretty-ranked (take kdm1a-directly-down 1))





(define report-on-gene
  (lambda (gene-curie)
    (displayln "\nRunning 2-hop down-up query with concept categories and drug safety:")
    (define q (time (query/graph
                  ((X       drug)
                   (Y       gene-or-protein)
                   (my-gene gene-curie)
                   (T       #f))
                  ((X->Y       negatively-regulates)
                   (Y->my-gene positively-regulates)
                   (X->T       drug-safe))
                  (X X->Y Y Y->my-gene my-gene)
                  (X X->T T))))

    (displayln "\nBuilding report:")
    (pretty-print (time (report/query q)))

    (displayln "\nRanking paths:")
    (define ranked (time (ranked-paths q)))

    (pretty-ranked (take ranked 1))))


;; alms1
(report-on-gene "HGNC:428")

;; rpgrip1l
(report-on-gene "HGNC:29168")

;; inpp5e
(report-on-gene "HGNC:21474")

;; nphp3
(report-on-gene "HGNC:7907")

;; kdm1a
(report-on-gene "HGNC:29079")
|#
