#lang racket
(provide (all-defined-out))
(require "../pieces-parts/query.rkt"         
         racket/engine)

#|NOTES FOR SYNONYMIZATION CACHE |#
#|
Safe Predicates to synonymize with
-   rtx2_2020_09_16 (487 . "equivalent_to")
-   rtx2_2020_09_16 (228 . "gene_encodes_gene_product")
-   rtx2_2020_09_16 (737 . "has_gene_product")
-   pr-owl (2 . "biolink:same_as")
-   pr-owl (3 . "biolink:has_gene_template*")
|#

#|test queries|#
;; "biolink:positively_regulates_entity_to_entity"
;; "biolink:negatively_regulates_entity_to_entity"

(define predicate:neg-reg/text-mining
  (list "biolink:negatively_regulates_entity_to_entity"))

(define predicate:pos-reg/text-mining
  (list "biolink:positively_regulates_entity_to_entity"))

(define predicate:co-occur/text-mining
  (list "biolink:related_to"))

(define test-ls
  '("HP:0006625"
    "HP:0100013"
    "HP:0100783"))

;; - get co-occurs edges HP --> PR
;; - transform PR: to HGNC with PR: --biolink_has_gene_template*--> HGNC

(define gene/gene-product '("biolink:GeneOrGeneProduct"))
(define drug '("biolink:ChemicalSubstance"))

(define test-query/drug->disease
  (map (lambda (curie)
            (query/graph
             ((S drug)
              (O curie))
             ((S->O #f) (edge/db? #f))
             (S S->O O)))
       breast-cancer/MONDO+HPO))

(define test-query/edges:drug->disease
  (map (lambda (edge) (edges/query edge 'S->O)) test-query/drug->disease))

(define test-query/disease->gene
  (map (lambda (curie)
            (query/graph
             ((S curie)
              (O gene/gene-product))
             ((S->O #f) (edge/db? #f))
             (S S->O O)))
       ;breast-cancer/MONDO+HPO
       test-ls
       ))

(define test-query/edges:disease->gene
  (map (lambda (edge) (edges/query edge 'S->O)) test-query/disease->gene))

(define get-PR-curies
  (lambda (query-result)
      (for-each
        (map concept->curie
           (map edge->object query-result)))))





;; scrape object out of HP->PR edges
;; 




#|

|#

;; X co-occurs--> breast-cancer MONDO
#;(edges/query
 (query/graph
  ((X gene/protein-concept?)
   (TC "MONDO:0004989"))
  ((TC->X (list "biolink:related_to")))
  (TC TC->X X))
 'TC->X)

#|
co-occurence breast cancer disease concepts

|#



#|
minikanren queries for edge types
(time
 (run 1 (eid subject object)
  (fresh (eprops)
    (edgeo
     `(rtx2_2020_09_16 ,eid ,subject ,object (228 . "gene_encodes_gene_product") ,eprops)))))

|#

