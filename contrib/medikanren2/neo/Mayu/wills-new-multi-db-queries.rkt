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

;; RTX KG2:
;; (query:X->Known #f '("biolink:treats") (list "DOID:9351"))

;; RTX KG2:
;; (query:X->Known #f '("biolink:subclass_of") (list "DOID:9351"))

;; Robokop:
;; (query:X->Known #f '("biolink:subclass_of") (list "NCBITaxon:1748027"))

;; Text Mining
;; (query:X->Known #f '("biolink:entity_negatively_regulates_entity") (list "UniProtKB:P47712"))

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

(filter (lambda (e) (string=? "water" (cadr e))) diabetes-causes)
