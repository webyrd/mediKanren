#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers.rkt"
 racket/match
 racket/set
 racket/pretty
 racket/string)

;; What diseases are caused by, or associated with, the BRCA2 gene?
(define brca2diseases
  (query:Known->X
   ;; list of known CURIES (for genes in this case)
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db
      (list "HGNC:1101" ;; BRCA2 gene
            ))))
   ;; list of predicates
   (set->list
    (set-union
     (get-predicate-descendents-in-db "biolink:causes")
     (get-predicate-descendents-in-db "biolink:gene_associated_with_condition")))
   ;; list of concept categories
   (set->list (get-class-descendents-in-db "biolink:Disease"))))

(define brca2disease-names
  (map
    (lambda (x) (list-ref x 4))
    brca2diseases))


(define top-level-diabetes-curies
  (curies->synonyms-in-db
   (list "DOID:9351")))

(define doid-diabetes-curies
  (get-descendent-curies*-in-db (list "DOID:9351")))

(define all-diabetes-curies
  (get-descendent-curies*-in-db
   (curies->synonyms-in-db
    (list "DOID:9351"))))
