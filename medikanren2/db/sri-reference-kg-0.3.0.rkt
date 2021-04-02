#lang racket/base
(provide
  tabled-relations nodes
)
(require "../base.rkt" (except-in racket/match ==))

(define columns-of-nodes 
'(
    id name category description xref ;0-4
    provided_by synonym anonymous clique_leader comment ;5-9
    created deprecated election_strategy gene_associated_with_condition genomic_sequence_localization_object
    has_url has_value identifier in_taxon invalid_biolink_category ;15-19
    iri mentions position same_as some_values_from ; 20-24
    source_version subsets title type _invalid_category) ;25-29
)
(define stcolumns-of-nodes (map symbol->string columns-of-nodes))

(define-relation/table nodes
  'path               "sri-reference/0.3.0/nodes"
  'source-file-path   "sri-reference/0.3.0/sri-reference-kg-0.3.0_nodes_nocr.tsv"
  'source-file-header columns-of-nodes
  'attribute-names    '(id subject object)
  'attribute-types    '(string string string)
  'map/append          (value/syntax
                        (lambda (row)
                          (define id (car row))
                          (map
                            (lambda (k v) (list id k v))
                            (cdr stcolumns-of-nodes)
                            (cdr row))))
;  'tables             '((curie name value))
;  'indexes            '((name value))
  )

(define tabled-relations (list nodes))
