#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"
 json
 racket/format
 racket/list
 racket/match
 racket/set
 racket/pretty
 racket/string)

(define (concept->name curie)
  (caddar (remove-duplicates (filter (lambda (cl)
                                      (and (equal? (car cl) curie)
                                           (equal? (cadr cl) "name")))
                                    (query:Concept (list curie))))))

(define regulates-EGFR
  (time (query:X->Known
         #f
         (set->list
          (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
           '("biolink:regulates")))
         (set->list
          (get-descendent-curies*-in-db
           (curies->synonyms-in-db (list "HGNC:3236")))))))

(map
 (lambda (edge)
   (match edge
     (`(,subj ,pred ,obj . ,props)
      (list (concept->name subj)
            pred
            (concept->name obj)))))
 regulates-EGFR)
