#lang racket
(provide nodes cprop edge eprop)

#|
  Extract from source data and rebuild with:

    time python medikanren2/util/storage-size-workaround/s-cerevisiae-kg-ref-ll.py && \
    rm -rf medikanren2/data/yeast-sri-reference && \
    time racket -l errortrace -u medikanren2/db/yeast-sri-reference-kg-0.3.0.rkt
|#


(require "../base.rkt" (except-in racket/match ==))
(require "../string-search.rkt")

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

(define-relation/table cprop
  'path               "yeast-sri-reference-kg/1.0/nodes"
  'source-file-path   "upstream/yeast-sri-reference-kg-tsv/nodes.tsv"
  'source-file-header columns-of-nodes
  'attribute-names    '(id subject object)
  'attribute-types    '(string string string)
  'map/append          (value/syntax
                        (lambda (row)
                          (define id (car row))
                          (append-map
                           (lambda (k v)
                             (if (equal? v "")
                                 '()
                                 (list (list id k v))))
                           (cdr stcolumns-of-nodes)
                           (cdr row))))
  )
;(string-search-init-rel cprop)

(define nodes cprop)
#|


(define columns-of-edges
'(id subject edge_label object relation
  provided_by assertion_confidence_score comment created_on description
  frequency_of_phenotype has_evidence has_measurement_value has_quantifier has_sex_specificity
  onset source type xref
))
(define stcolumns-of-edges (map symbol->string columns-of-edges))

(define (edge-from-row row)
  (list
  (list-ref row 1)
  (list-ref row 3)))

(define (eprop-from-row row)
  (cons
  (list-ref row 2)
  (list-tail row 4)))

(define-relation/table edge
  'path               "yeast-sri-reference-kg/1.0/edge"
  'source-file-path   "upstream/yeast-sri-reference-kg-tsv/edges.tsv"
  'source-file-header columns-of-edges
  'attribute-names    '(id subject object)
  'attribute-types    '(string string string)
  'map/append          (value/syntax
                        (lambda (row)
                          (define id (car row))
                          (list (cons id (edge-from-row row)))))
  'indexes            '(                  ; default index is: id subject object
                        (subject object)  ; implicit: lookup ending in id
                        (object subject)) ; implicit: lookup ending in id
)

(define-relation/table eprop
  'path               "yeast-sri-reference-kg/1.0/eprop"
  'source-file-path   "upstream/yeast-sri-reference-kg-tsv/edges.tsv"
  'source-file-header columns-of-edges
  'attribute-names    '(id subject object)
  'attribute-types    '(string string string)
  'map/append          (value/syntax
                        (lambda (row)
                          (define id (car row))
                          (append-map
                           (lambda (k v)
                             (if (equal? v "")
                                 '()
                                 (list (list id k v))))
                           (eprop-from-row stcolumns-of-edges)
                           (eprop-from-row row))))
  'indexes            '(                  ; default index is: id subject object
                        (subject object)  ; implicit: lookup ending in id
                        (object subject)) ; implicit: lookup ending in id
)
|#

(define-relation/table edge
  'attribute-names    '(id subject object)
  'attribute-types    '(string string string)
  'source-stream '())

(define-relation/table eprop
  'attribute-names    '(id subject object)
  'attribute-types    '(string string string)
  'source-stream '())

(database-extend-relations!
  'yeast-sri-reference-kg-0.3.0
  'cprop cprop
  'edge edge
  'eprop eprop
  )
