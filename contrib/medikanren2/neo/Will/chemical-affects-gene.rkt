#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"
 "../../../../medikanren2/neo/neo-server/neo-server-utils.rkt"
 "../utils.rkt"
 json
 racket/format
 racket/list
 racket/match
 racket/set
 racket/pretty
 racket/string)

;; finds a list of compounds that affect a provided set of genes
(define (chem-affects-gene gene-list)
  (time (remove-duplicates
         (query:X->Known
          (set->list
           (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
            '("biolink:ChemicalEntity")))
          (set->list
           (get-non-deprecated/mixin/absreact-ins-and-descendent-predicates*-in-db
            '("biolink:affects")))
          (set->list (curie-synonyms-and-descendents gene-list))))))

;; test with BRCA2 gene
(define chem-affects-BRCA2 (chem-affects-gene (list "HGNC:1101")))
