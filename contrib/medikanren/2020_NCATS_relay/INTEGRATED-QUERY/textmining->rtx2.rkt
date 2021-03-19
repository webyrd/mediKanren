#lang racket
(provide (all-defined-out))
(require "../../../../medikanren/pieces-parts/query.rkt"         
         racket/engine)

(define crosskg-mondo-disease-curies
  '("MONDO:0016192"
    "DOID:0050890"
    "MONDO:0016153"
    "MONDO:0016147"
    "MONDO:0016191"
    "MONDO:0016196"
    "MONDO:0016187"))

(define CHEBI--negatively-regulates-->PR-->MONDO
  (curies/query
   (time (query/graph
          ((Drug #f)
           (PR #f)
           ;(Disease "MONDO:0000510")
           (Disease "MONDO:0016147")
           )
          ((Drug->PR (list "biolink:negatively_regulates_entity_to_entity") (edge/db? 'textminingprovider)) 
           (PR->Disease
            (list
             "gene_mapped_to_disease"
             "gene_involved_in_pathogenesis_of_disease"
             "gene_associated_with_disease"
             "associated_with_disease"
             "INVERTED:disease_has_basis_in_dysfunction_of") (edge/db? 'rtx2_2020_09_16)))
          (Drug Drug->PR PR)                         
          (PR PR->Disease Disease)))
   'Drug))

(define CHEBI--postively-regulates-->PR-->MONDO
  (curies/query
   (time (query/graph
          ((Drug #f)
           (PR #f)
           (Disease "MONDO:0000510"))
          ((Drug->PR (list "biolink:positively_regulates_entity_to_entity") (edge/db? 'textminingprovider)) 
           (PR->Disease
            (list
             "gene_mapped_to_disease"
             "gene_involved_in_pathogenesis_of_disease"
             "gene_associated_with_disease"
             "associated_with_disease"
             "INVERTED:disease_has_basis_in_dysfunction_of") (edge/db? 'rtx2_2020_09_16)))
          (Drug Drug->PR PR)               
          (PR PR->Disease Disease)))
   'Drug))

