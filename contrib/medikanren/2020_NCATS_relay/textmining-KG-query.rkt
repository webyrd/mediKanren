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

(define predicate:PR->HGNC
  '("biolink:has_gene_template*"))

;; breast cancer HP terms
;; co-occurence KG requires HP terms for Disease->Gene Associations
(define test-ls
  '("HP:0006625"
    "HP:0100013"
    "HP:0100783"))

;; - get co-occurs edges HP --> PR
;; - transform PR: to HGNC with PR: --biolink_has_gene_template*--> HGNC

(define gene/gene-product '("biolink:GeneOrGeneProduct"))

(define drug '("biolink:ChemicalSubstance"))

;; HP --> PR query
(define test-query/disease->gene
  (map (lambda (curie)
            (query/graph
             ((S curie)
              (O gene/gene-product))
             ((S->O #f) (edge/db? #f))
             (S S->O O)))       
       test-ls
       ))

;; HP --> PR edges
(define test-query/edges:disease->gene
  (map (lambda (edge) (edges/query edge 'S->O)) test-query/disease->gene))

;; scrape object out of HP->PR edges, and requery PR codes in the PR-OWL KG for HGNCs

(define x
  (map
   (lambda (edge-ls)
     (map (lambda (edge)
            (let ((PR-curie (concept->curie (edge->object edge))))
              (printf "~a\n" PR-curie)
              #;(filter (lambda (object) (string-)))
              (curies/query
                (query/graph
                 ((S PR-curie)
                  (O #f))
                 ((S->O '("biolink:in_taxon")) (edge/db? #f))
                 (S S->O O))
                'O)))
          edge-ls))
   test-query/edges:disease->gene)) 

(edges/query
 (query/graph
  ((S "PR:P30042")
   (O '("biolink:OrganismTaxon")))
  ((S->O #f) (edge/db? #f))
  (S S->O O)) 'S->O) 










#|
;; lots of noise/non-specific drug findings in these edges 
(define test-query/drug->disease
  (map (lambda (curie)
            (query/graph
             ((S drug)
              (O curie))
             ((S->O #f) (edge/db? #f))
             (S S->O O)))
       test-ls
       ))

;; CHEBI -> PR edges 
(define test-query/edges:drug->disease
  (map (lambda (edge) (edges/query edge 'S->O)) test-query/drug->disease))
|#









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

