#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers.rkt"
 racket/match
 racket/set
 racket/pretty
 racket/string)

;; Problem encountered on 5 Dec 2022:
;;
;; * HGNC CURIES are not normalized to UMLS or NCBI or OMIM CURIES

(define HGNC-gene-curie-symbols '(HGNC:6884 HGNC:22082 HGNC:8032 HGNC:25088 HGNC:12009 HGNC:17997 HGNC:26509 HGNC:11254 HGNC:12855 HGNC:6192 HGNC:15968 HGNC:27230 HGNC:16493 HGNC:10542 HGNC:8851 HGNC:6323 HGNC:6482 HGNC:3811 HGNC:13450 HGNC:29433 HGNC:11581 HGNC:23537 HGNC:23166 HGNC:23159 HGNC:15573))

(define HGNC-gene-curies (map symbol->string HGNC-gene-curie-symbols))

(define get-generic-synonym/descendent-curies
  (lambda (curie)
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db (list curie)))))

(define get-gene-synonym/descendent-curies
  (lambda (curie)
    (let ((related-curies (get-generic-synonym/descendent-curies curie))
          (gene-synonym-predicates
           (set->list
            (apply set-union
                   (map get-predicate-descendents-in-db
                        '("biolink:same_as"
                          "biolink:expresses"
                          "biolink:has_gene_product"
                          "biolink:has_gene_or_gene_product"
                          "biolink:has_output"
                          "biolink:exact_match"
                          "biolink:mesh_terms"
                          "biolink:xref"))))))
      (set-union
       related-curies
       (list->set
        (map (lambda (e) (list-ref e 3))
             (query:Known->X
              (set->list related-curies)
              gene-synonym-predicates
              #f)))
       (list->set
        (map car
             (query:X->Known
              #f
              gene-synonym-predicates
              (set->list related-curies))))))))

(define process-gene
  (lambda (curie)
    (let ((gene-curies (get-gene-synonym/descendent-curies curie)))
      (query:X->Known
       ;; Looking for any type of chemical substance
       #f
       ;; list of predicates
       (set->list
        (apply set-union
               (map get-predicate-descendents-in-db
                    '("biolink:positively_regulates"
                      "biolink:negatively_regulates"
                      ;;
                      "biolink:entity_positively_regulates_entity"
                      "biolink:entity_negatively_regulates_entity"
                      ;;
                      "biolink:increased_amount_of"
                      "biolink:increases_activity_of"
                      "biolink:increases_amount_or_activity_of"
                      "biolink:increases_abundance_of"
                      "biolink:increases_expression_of"
                      "biolink:increases_synthesis_of"
                      ;;
                      "biolink:decreased_amount_in"
                      "biolink:decreases_abundance_of"
                      "biolink:decreases_synthesis_of"
                      "biolink:decreases_activity_of"
                      "biolink:decreases_amount_or_activity_of"
                      "biolink:decreases_expression_of"))))
       ;; list of known CURIES (for genes)
       (set->list gene-curies)))))

(process-gene "UMLS:C0598034")
