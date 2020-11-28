#lang racket/base
(require "common.rkt" (except-in racket/match ==) racket/pretty)

;; TODO: this might be useful later
;(materialize-relation
;  'path               "semmed/concept"
;  'source-file-path   "semmed/semmed.node.csv"
;  'source-file-header '(":ID")
;  'attribute-names    '(curie)
;  'attribute-types    '(string))

(materialize-relation
  'path               "semmed/cprop"
  'source-file-path   "semmed/semmed.nodeprop.csv"
  'source-file-header '(:ID propname value)
  'attribute-names    '(curie key value)
  'attribute-types    '(string string string)
  'tables             '((curie key value))
  'indexes            '((key value)
                        (value)))

(materialize-relation
  'path               "semmed/edge"
  'source-file-path   "semmed/semmed.edge.csv"
  'source-file-header '(":ID" ":START" ":END")
  'transform          (lambda (row)
                        (match-define (list id subject object) row)
                        (list (string->number id) subject object))
  'attribute-names    '(id subject object)
  'attribute-types    '(nat string string)
  'tables             '((subject object id))
  'indexes            '((object subject)
                        (id)))

(materialize-relation
  'path               "semmed/eprop"
  'source-file-path   "semmed/semmed.edgeprop.csv"
  'source-file-header '(":ID" "propname" "value")
  'transform          (lambda (row)
                        (match-define (list id key value) row)
                        (list (string->number id) key value))
  'attribute-names    '(id key value)
  'attribute-types    '(nat string string)
  'indexes            '((key value)
                        (value)))

(materialize-relation
  'path               "semmed/eprop"
  'source-file-path   "semmed/semmed.edgeprop.csv"
  'source-file-header '(":ID" "propname" "value")
  'transform          (lambda (row)
                        (match-define (list id key value) row)
                        (list (string->number id) key value))
  'attribute-names    '(id key value)
  'attribute-types    '(nat string string)
  'indexes            '((key value)
                        (value)))

(define predicates
  '("affects" "causes" "coexists_with" "derives_into"
    "gene_associated_with_condition" "interacts_with"
    "location_of" "manifestation_of"
    "negatively_regulates" "part_of" "positively_regulates"
    "precedes" "predisposes" "prevents" "produces"
    "related_to" "subclass_of" "treats"))

(time (let ()
        (define-materialized-relation
          cprop '((path           . "semmed/cprop")
                  ;; specifying retrieval-type is optional (default is disk)
                  (retrieval-type . disk)
                  ;(retrieval-type . bytes)
                  ;(retrieval-type . scm)
                  ))

        (define-materialized-relation edge  '((path . "semmed/edge")))
        (define-materialized-relation eprop '((path . "semmed/eprop")))

        (time (pretty-print
                (run 10 (curie name)
                  (cprop curie "category" "chemical_substance")
                  (cprop curie "name" name))))
        (newline)

        (time (pretty-print
                (run 10 (curie1 predicate curie2)
                  (fresh (eid)
                    (eprop eid "edge_label" predicate)
                    (edge eid curie1 curie2)))))
        (newline)

        ;; TODO: fix performance issue related to constraint ordering
        (time (pretty-print
                (run 10 (curie1 k1 v1 curie2 k2 v2)
                  (fresh (eid)
                    (eprop eid "edge_label" "negatively_regulates")
                    (edge eid curie1 curie2)
                    (cprop curie1 k1 v1)
                    (cprop curie2 k2 v2)))))
        (newline)
        ))
