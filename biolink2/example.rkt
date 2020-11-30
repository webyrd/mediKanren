#lang racket/base
(require "common.rkt" (except-in racket/match ==) racket/pretty)

(time (let ()
        ;; TODO: this might be useful later
        ;(define-materialized-relation concept
          ;'path               "semmed/concept"
          ;'source-file-path   "semmed/semmed.node.csv"
          ;'source-file-header '(":ID")
          ;'attribute-names    '(curie)
          ;'attribute-types    '(string))

        (define-materialized-relation cprop
          'path               "semmed/cprop"
          'source-file-path   "semmed/semmed.nodeprop.csv"
          'source-file-header '(:ID propname value)
          'attribute-names    '(curie key value)
          'attribute-types    '(string string string)
          'tables             '((curie key value))
          'indexes            '((key value)
                                (value))
          ;; specifying retrieval-type is optional (default is disk)
          'retrieval-type     'disk
          ;'retrieval-type     'bytes
          ;'retrieval-type     'scm
          )

        (define-materialized-relation edge
          'path               "semmed/edge"
          'source-file-path   "semmed/semmed.edge.csv"
          'source-file-header '(":ID" ":START" ":END")
          'transform          (lambda (row)
                                (match-define (list id subject object) row)
                                (list (string->number id) subject object))
          'attribute-names    '(id subject object)
          'attribute-types    '(nat string string)
          'indexes            '((subject object)
                                (object subject)))

        (define-materialized-relation eprop
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

        (time (pretty-print
                (run 10 (eid curie1 name1 curie2 name2)
                  (fresh (_)
                    (eprop eid "edge_label" "negatively_regulates")
                    (edge eid curie1 curie2)
                    (cprop curie1 "name" name1)
                    (cprop curie2 "name" name2)))))
        (newline)

        (displayln "All concept property keys:")
        (time (pretty-print
                (run* (k)
                  (fresh (curie v)
                    (cprop curie k v)))))
        (newline)

        (displayln "All edge property keys:")
        (time (pretty-print
                (run* (k)
                  (fresh (eid v)
                    (eprop eid k v)))))
        (newline)

        (displayln "All concept categories:")
        (time (pretty-print
                (run* (category)
                  (fresh (curie)
                    (cprop curie "category" category)))))
        (newline)

        (displayln "All edge predicates:")
        (time (pretty-print
                (run* (predicate)
                  (fresh (eid)
                    (eprop eid "edge_label" predicate)))))
        (newline)
        ))
