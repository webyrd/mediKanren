#lang racket/base
(provide concept cprop edge eprop)
(require "../base.rkt" (except-in racket/match ==))

;; TODO: this might be useful later
;(define-relation/table concept
  ;'path               "semmed/concept"
  ;'source-file-path   "semmed/semmed.node.csv"
  ;'source-file-header '(":ID")
  ;'attribute-names    '(curie)
  ;'attribute-types    '(string))
(define-relation (concept curie)
  (fresh (k v)
    (cprop curie k v)))

(define-relation/table cprop
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

(define-relation/table edge
  'path               "semmed/edge"
  'source-file-path   "semmed/semmed.edge.csv"
  'source-file-header '(":ID" ":START" ":END")
  'map                (value/syntax
                        (lambda (row)
                          (match-define (list id subject object) row)
                          (list (string->number id) subject object)))
  'attribute-names    '(id subject object)
  'attribute-types    '(nat string string)
  'indexes            '((subject object)
                        (object subject)))

(define-relation/table eprop
  'path               "semmed/eprop"
  'source-file-path   "semmed/semmed.edgeprop.csv"
  'source-file-header '(":ID" "propname" "value")
  'map                (value/syntax
                        (lambda (row)
                          (match-define (list id key value) row)
                          (list (string->number id) key value)))
  'attribute-names    '(id key value)
  'attribute-types    '(nat string string)
  'indexes            '((key value)
                        (value)))
