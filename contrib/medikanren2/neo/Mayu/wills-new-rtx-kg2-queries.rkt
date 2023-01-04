#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers.rkt"
 json
 racket/format
 racket/list
 racket/match
 racket/set
 racket/pretty
 racket/string)

(define diabetes-treatments
  (query:X->Known
   #f
   (set->list
    (apply set-union
           (map get-predicate-descendents-in-db
                '("biolink:treats"
                  ))))
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db
      (list "DOID:9351"))))))

(length diabetes-treatments)
;; 17247


(define diabetes-causes
  (query:X->Known
   #f
   (set->list
    (apply set-union
           (map get-predicate-descendents-in-db
                '("biolink:causes"
                  ))))
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db
      (list "DOID:9351"))))))

(length diabetes-causes)

(sort (remove-duplicates (map car diabetes-causes)) string<=?)
