#lang racket/base
(require "common.rkt" racket/pretty)

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
  'source-file-header '(":ID" "propname" "value")
  'attribute-names    '(curie key value)
  'attribute-types    '(string string string)
  'indexes            '((key value)
                        (value)))

(materialize-relation
  'path               "semmed/edge"
  'source-file-path   "semmed/semmed.edge.csv"
  'source-file-header '(":ID" ":START" ":END")
  'attribute-names    '(id subject object)
  'attribute-types    '(string string string)
  'tables             '((subject object id))
  'indexes            '((object subject)
                        (id)))

(materialize-relation
  'path               "semmed/eprop"
  'source-file-path   "semmed/semmed.edgeprop.csv"
  'source-file-header '(":ID" "propname" "value")
  'attribute-names    '(id key value)
  'attribute-types    '(string string string)
  'indexes            '((key value)
                        (value)))

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
               (run 10 (curie1 predicate curie2)
                    (fresh (eid)
                       (eprop eid "edge_label" predicate)
                       (edge eid curie1 curie2)))))
        (newline)

        (time (pretty-print
               (run 10 (curie1 k1 v1 curie2 k2 v2)
                    (fresh (id)
                       (eprop id "edge_label" "biolink:has_gene_product")
                       (edge id curie1 curie2)
                       (cprop curie1 k1 v1)
                       (cprop curie2 k2 v2)))))
        (newline)

        (time (pretty-print
                (run 600 (curie name)
                  (cprop curie "category" "chemical_substance")
                  (cprop curie "name" name))))
        (newline)
        ))
