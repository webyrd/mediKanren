#lang racket
(require "query.rkt")
                 
(define spike-covid19 "UniProtKB:P0DTC2")

(define spike-sars "UniProtKB:P59594")

(define ace2 "UMLS:C1422064")

(define covid19 "MONDO:0100096")

(define sars "CUI:C1175743")

(define mers "CUI:C3698360")

(define (sort-pubs pubs)
  (sort
   (hash-map pubs (lambda (k v) (cons k v)))
   >
   #:key (lambda (x) (set-count (cdr x)))))

(define (sort-count c)
  (sort
   (hash-map c (lambda (k v) (cons k v)))
   >
   #:key (lambda (x) (cdr x))))

#|
takes: a curie of interest (c), a list of type labels (eg. from query.rkt: drug, gene, etc.) (result-filter) - enter #f if no filter
q1 does query C-"biolink:related_to"->P where C is the curie of interest (c), and P is a publication concept.
q2 does a separate query C-"biolink:related_to"->P where C is either open (#f) or filtered by result-filter, and
P is the list of publication curies resulting from the P in q1.
purpose: shows the frequency of co-occurrence between different concepts in publications, this gives a sense of
         what concept curies are often mentioned together in biomedical literature
returns: an association list where the keys are concept curies (x) (filtered by result-filter), and the values are sets of all
         publications that mention both the concept curie (x) and the curie of interest (c). The assoc list is sorted (high->low)
         by the number of publications in which the concept curie (x) and the curie of interest (c) co-occur
|#
(define (cluster-concepts-by-pubs c result-filter)
  (define q1 (query/graph
              ((C c)
               (P #f))
              ((C->P  '("biolink:related_to")))
              (C C->P P)))
  (define related-pubs (curies/query q1 'P))
  (define publications (make-hash))
  (define (step2 p)
    (define q2 (time (query/graph
                      ((C result-filter)
                       (P p))
                      ((C->P  '("biolink:related_to")))
                      (C C->P P))))
    (define related-concepts (curies/query q2 'C))
    (for-each
     (lambda (x)
       (unless (equal? x c) 
         (hash-update! publications x (lambda (s) (set-add s p)) (set))
         )
       )
     related-concepts)
    )
  (for-each step2 related-pubs)
  (sort-pubs publications)
  )
#|Examples
;;quick examples: these curies have fewer "biolink:related_to" edges, so they take a lot less time to finish running
(cluster-concepts-by-pubs "NCBIGene:3852" #f))
(cluster-concepts-by-pubs "CUI:C1418795" #f))

;;longer, covid related examples:

;;finds all curies (no filter) that are mentioned in the same publication as the sars spike protein and all
  the publications in which this occurs
(define sars-spike-gene-cluster (cluster-concepts-by-pubs "UniProtKB:P59594" #f))

(define spike-covid19 "UniProtKB:P0DTC2")
(define covid-spike-gene-cluster (cluster-concepts-by-pubs spike-covid19 gene))

(define ace2-drug-cluster (cluster-concepts-by-pubs "CUI:C1422064" drug))
|#

#|
takes: the association list returned by cluster-concepts-by-pred
returns: a sorted assoc list where the value is just the count of the number of publications rather than a set
         of the actual publication curies
|#
(define (pubs->count cluster)
  (map
   (lambda (x)
     (cons (car x) (set-count (cdr x)))
     )
   cluster
   )
  )
#|Examples
(pubs->count sars-spike-gene-cluster)
(pubs->count covid-spike-gene-cluster)
(pubs->count ace2-drug-cluster)
|#