#lang racket/base
(provide concept cprop edge eprop
         subclass-of subclass-of+ subclass-of*)
(require "../base.rkt" (except-in racket/match ==))
(require "../string-search.rkt")

(define-relation (concept curie)
  (fresh (k v)
    (cprop curie k v)))

(define-relation/table cprop
  'path               "rtx-kg2-lines1000/1.0/cprop"
  'source-file-path   "upstream/rtx-kg2-lines1000/rtx_kg2.nodeprop.tsv"
  'source-file-header '(:ID propname value)
  'attribute-names    '(curie key value)
  'attribute-types    '(string string string)
  'tables             '((curie key value))
  'indexes            '((key value)))

(string-search-init-rel cprop)

(define-relation/table edge
  'path               "rtx-kg2-lines1000/1.0/edge"
  'source-file-path   "upstream/rtx-kg2-lines1000/rtx_kg2.edge.tsv"
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
  'path               "rtx-kg2-lines1000/1.0/eprop"
  'source-file-path   "upstream/rtx-kg2-lines1000/rtx_kg2.edgeprop.tsv"
  'source-file-header '(":ID" "propname" "value")
  'map                (value/syntax
                        (lambda (row)
                          (match-define (list id key value) row)
                          (list (string->number id) key value)))
  'attribute-names    '(id key value)
  'attribute-types    '(nat string string)
  'indexes            '((key value)))

(define-relation (subclass-of a b)
  (fresh (eid)
    (eprop eid "predicate" "biolink:subclass_of")
    (edge eid a b)))

(define-relation (subclass-of+ a b)
  (conde ((subclass-of a b))
         ((fresh (mid)
            (subclass-of mid b)
            (subclass-of+ a mid)))))

(define-relation (subclass-of* a b)
  (conde ((== a b))
         ((subclass-of+ a b))))

(database-extend-relations!
  'rtx2-20210204
  'cprop cprop
  'eprop eprop
  'edge  edge)