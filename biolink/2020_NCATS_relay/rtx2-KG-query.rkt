#lang racket
(provide (all-defined-out))
(require "../pieces-parts/query.rkt"         
         racket/engine)

(define test-ls
  '("MONDO:0000618"
    "MONDO:0004989"
    "MONDO:0021100"
    "NCIT:C4872"
    "UMLS:C0678222"
    "MESH:D001943"))

(define test-query/gene->disease
  (time
   (map (lambda (curie) 
            (query/graph
             ((S #f)
              (O curie))
             ((S->O (list "gene_mapped_to_disease"
                          "gene_involved_in_pathogenesis_of_disease"
                          "gene_associated_with_disease"
                          "associated_with_disease"
                          "INVERTED:disease_has_basis_in_dysfunction_of")) (edge/db? #f))
             (S S->O O)))       
       test-ls)))

(define test-query/edges:gene->disease
  (map (lambda (edge) (edges/query edge 'S->O))
       test-query/gene->disease))






;; working query 
(define test-query/drug->disease
  (time
   (map (lambda (curie) 
            (query/graph
             ((S #f)
              (O curie))
             ((S->O (list "treats")) (edge/db? #f))
             (S S->O O)))       
       test-ls)))

(define test-query/test-query/drug->disease
  (map (lambda (edge) (edges/query edge 'S->O))
       test-query/drug->disease))


