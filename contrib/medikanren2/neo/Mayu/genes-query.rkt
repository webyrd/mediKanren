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

(define HGNC/symbol/UMLS*-list
  '((HGNC:6884  MAPK8IP3 C1417026 C1436798)
    (HGNC:22082 VMA21    C2681112 C2715590)
    (HGNC:8032  NTRK2    C1334909 C1704842 C3853697)
    (HGNC:25088 SGO1     C1428445 C1565925 C3890006)
    (HGNC:12009 TPI1     C1420871 C3887669 C3541335 C0041078)
    (HGNC:17997 FKRP     C1425226 C1448221)
    (HGNC:26509 PHETA1   C1825239 C2932936)
    (HGNC:11254 SPOP     C1420368 C3540803 C0669183)
    (HGNC:12855 YWHAZ    C1421564 C3492243 C2981740)
    (HGNC:6192  JAK2     C1334291 C1527617 C0169661)
    (HGNC:15968 GDAP1    C1423872 C1453327)
    (HGNC:27230 ESCO2    C1539367 C1569535)
    (HGNC:16493 ZMIZ1    C1823956 C1433857 C3889263)
    (HGNC:10542 SBF1     C1419823 C1259214)
    (HGNC:8851  PEX10    C1418470 C1449205)
    (HGNC:6323  KIF5A    C1416638 C1569075)
    (HGNC:6482  LAMA2    C1416776 C1569076)
    (HGNC:3811  FOXG1    C0812297 C1705148)
    (HGNC:13450 BANP     C1538317 C0961071)
    (HGNC:29433 NEXMIF   C1845260 C4041927)
    (HGNC:11581 TBCD     C1420596 C1447856)
    (HGNC:23537 DHTKD1   C1428130 C3657993)
    (HGNC:23166 PNPT1    C1427960 C1310854 C2984134)
    (HGNC:23159 ALG2     C1427955)
    (HGNC:15573 SETBP1   C1423585 C1309881 C3815268)))

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
