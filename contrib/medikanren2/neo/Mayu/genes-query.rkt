#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers.rkt"
 racket/match
 racket/set
 racket/pretty
 racket/string)

;; Problems encountered on 5 Dec 2022:
;;
;; * HGNC CURIES were not normalized to UMLS or NCBI or OMIM CURIES
;;
;; * We couldn't find any biolink:positively_regulates or
;; biolink_negatively_regulates edges for the genes

(define gene-curie-symbols '(HGNC:6884 HGNC:22082 HGNC:8032 HGNC:25088 HGNC:12009 HGNC:17997 HGNC:26509 HGNC:11254 HGNC:12855 HGNC:6192 HGNC:15968 HGNC:27230 HGNC:16493 HGNC:10542 HGNC:8851 HGNC:6323 HGNC:6482 HGNC:3811 HGNC:13450 HGNC:29433 HGNC:11581 HGNC:23537 HGNC:23166 HGNC:23159 HGNC:15573))

(define gene-curies (map symbol->string gene-curie-symbols))

;; function for synonyms
(define get-related-curies
  (lambda (curie)
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db (list curie)))))

(define process-gene
  (lambda (curie)
    (let ((related-curies (get-related-curies curie)))
      (query:X->Known
       ;; Looking for any type of chemical substance
       #f
       ;; list of predicates
       (set->list
        (set-union
         (get-predicate-descendents-in-db "biolink:positively_regulates")
         (get-predicate-descendents-in-db "biolink:negatively_regulates")))
       ;; list of known CURIES (for genes)
       (set->list related-curies)))))

(define process-drug
  (lambda (curie)
    (let ((related-curies (get-related-curies curie)))
      (query:Known->X
       ;; Looking for any type of chemical substance
       (set->list related-curies)
       ;; list of predicates
       (set->list
        (set-union
         (get-predicate-descendents-in-db "biolink:positively_regulates")
         (get-predicate-descendents-in-db "biolink:negatively_regulates")))
       ;; list of known CURIES (for genes)
       #f))))


