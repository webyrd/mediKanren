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

;; Hand-curated mappings of HGNC to UMLS mappings, since there doesn't
;; seem to currently be a way to map HGNC CURIES to UMLS CURIES
;; through RTX-KG2 or through the node normalization knowledge graph.
(define symbol/HGNC/UMLS*-list
  '((MAPK8IP3  HGNC:6884   C1417026 C1436798)
    (VMA21     HGNC:22082  C2681112 C2715590)
    (NTRK2     HGNC:8032   C1334909 C1704842 C3853697)
    (SGO1      HGNC:25088  C1428445 C1565925 C3890006)
    (TPI1      HGNC:12009  C1420871 C3887669 C3541335 C0041078)
    (FKRP      HGNC:17997  C1425226 C1448221)
    (PHETA1    HGNC:26509  C1825239 C2932936)
    (SPOP      HGNC:11254  C1420368 C3540803 C0669183)
    (YWHAZ     HGNC:12855  C1421564 C3492243 C2981740)
    (JAK2      HGNC:6192   C1334291 C1527617 C0169661)
    (GDAP1     HGNC:15968  C1423872 C1453327)
    (ESCO2     HGNC:27230  C1539367 C1569535)
    (ZMIZ1     HGNC:16493  C1823956 C1433857 C3889263)
    (SBF1      HGNC:10542  C1419823 C1259214)
    (PEX10     HGNC:8851   C1418470 C1449205)
    (KIF5A     HGNC:6323   C1416638 C1569075)
    (LAMA2     HGNC:6482   C1416776 C1569076)
    (FOXG1     HGNC:3811   C0812297 C1705148)
    (BANP      HGNC:13450  C1538317 C0961071)
    (NEXMIF    HGNC:29433  C1845260 C4041927)
    (TBCD      HGNC:11581  C1420596 C1447856)
    (DHTKD1    HGNC:23537  C1428130 C3657993)
    (PNPT1     HGNC:23166  C1427960 C1310854 C2984134)
    (ALG2      HGNC:23159  C1427955)
    (SETBP1    HGNC:15573  C1423585 C1309881 C3815268)))

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

(define process-symbol/HGNC/UMLS*-list
  (lambda (symbol/HGNC/UMLS*-list)
    (let loop ((ls symbol/HGNC/UMLS*-list))
      (match ls
        ['() '()]
        [`((,symbol ,hgnc-curie-symbol . ,cid-curie-symbols) . ,rest)
         (let ((hgnc-curie (symbol->string hgnc-curie-symbol))
               (cid-curies (map symbol->string cid-curie-symbols)))
           (let ((umls-curies (map (lambda (c) (string-append "UMLS:" c)) cid-curies)))
             (let ((answers (apply append (map process-gene (cons hgnc-curie umls-curies)))))
               (cons `(,symbol (,hgnc-curie-symbol . ,cid-curie-symbols) ,answers)
                     (loop rest)))))]))))
