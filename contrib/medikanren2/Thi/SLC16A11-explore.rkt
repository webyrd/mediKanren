#lang racket

(require "../../../medikanren2/common.rkt")
; only load rtx2 data in config.scm

(define hispanic "UMLS:C0086409")

; a use case: Find all genes/proteins that are one-hop away from this hispanic curie
; Subject = gene-or-protein (can be defined with concepts' categories)
; Object = a particular curie
; predicate = #f

; use the "run"  (minikanren's interface) to find anything related to this curie (limit to 10 entries)
(run 10 (id s)
  (edge id s hispanic))

;; define the gene-or-protein categories with biolink categories

(define gene-or-protein '("biolink:Gene"
                          "biolink:GeneFamily"
                          "biolink:GeneProduct"
                          "biolink:GenomicEntity"
                          "biolink:MacromolecularComplex"
                          "biolink:MolecularEntity"
                          "biolink:Protein"))

;; helper functions:

(define (properties->hash kvs)
  (make-immutable-hash
   (map (lambda (kv) (cons (car kv) (cadr kv)))
        kvs)))


(define-relation (concept/category curie categories)
  (fresh (category)
    (cprop curie "category" category)
    (membero category categories)))

(define-relation (edge/properties s s->o o)
  (fresh (s->o:id)
    (edge s->o:id s o)
    (:== s->o (s->o:id)
         (run* (key value)
           (eprop s->o:id key value)))))

#;(define (makeshift-query/graph s o)
  (map properties->hash
       (run 10 s->o:properties
         (fresh (s o)
           (== o hispanic)
           (constrain/categories s gene-or-protein)
           (edge/properties s->o:properties s o)
           ))))


;;(define list-of-curies (list "HGNC:10928" "HGNC:23093" "HGNC:10922"))

;; WISH-list: a higher level function that takes inputs: could be S, O (either curies, or list of curies
;; or a list of categories, or #f) or S->O (single predicate, list of predicates, or #f)
;; Output: 1/summary of results (it could be count of entries, count of S, O and Edges, such as report/query in medikanren1.
;; or it could be a tsv export for medium-level summary) 2/verbose results so that we can look at the details if needed


;; this function could be generalized

(define gene-hispanic
  (time (map (lambda (row)
               (match-define (list s->o s:name o:name) row)
               (hash-set (hash-set (properties->hash s->o)
                                   'subject-name s:name)
                         'object-name o:name))
             (run* (s->o s:name o:name)
               (fresh (s o)
                                        ;(== o hispanic)
                 (membero o list-of-curies)
                 (concept/category s gene-or-protein)
                 (cprop o "name" o:name)
                 (cprop s "name" s:name)
                 (edge/properties s s->o o))))))



(length gene-hispanic)

(length (remove-duplicates (map (lambda (ans) (hash-ref ans "subject")) gene-hispanic)))

(map (lambda (ans) (hash-ref ans "predicate")) gene-hispanic)
(map (lambda (ans) (hash-ref ans 'subject-name)) gene-hispanic)

(run* (key value) (cprop "HGNC:23093" key value))



