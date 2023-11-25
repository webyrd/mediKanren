#lang racket/base

(provide concept->name
         concept->category

         curie-synonyms-and-descendents

         write-answers-to-tsv

         get-publications
         
         get-source
         num-pubs
         edge-has-source?
         
         get-assoc
         list-assoc
         merge-list
         merge-hash   
         )

(require racket/list
         racket/math
         racket/string
         json
         racket/match
         "../../../medikanren2/neo/neo-server/neo-server-utils.rkt"
         "../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
         "../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"         
         "../../../medikanren2/neo/neo-utils/neo-helpers-without-db.rkt"
         )

(define (curie-synonyms-and-descendents curie-list)
  (get-descendent-curies*-in-db
   (curies->synonyms-in-db curie-list)))

(define (concept->name curie)
  (let ((id-name-val
         (remove-duplicates (filter (lambda (cl)
                                      (and (equal? (car cl) curie)
                                           (equal? (cadr cl) "name")))
                                    (query:Concept (list curie))))))
        (if (null? id-name-val)
            curie
            (caddar id-name-val))))

(define (concept->category curie)
  (let ((category (assoc "category" (curie->properties curie))))
    (if category (cdr category) '())))

(define (write-answers-to-tsv edges)
  'TODO)
