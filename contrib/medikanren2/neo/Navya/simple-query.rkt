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

(define diabetes-causes
  (query:X->Known
   #f
   (set->list
    (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
     '("biolink:causes")))
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db
      (list "DOID:9351"))))))

(define diabetes-treatments
  (query:X->Known
   #f
   (set->list
    (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
     '("biolink:treats")))
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db
      (list "DOID:9351"))))))

(curies->synonyms-in-db (list "PUBCHEM.COMPOUND:5291"))
;; =>
'("PUBCHEM.COMPOUND:5291"
  "PUBCHEM.COMPOUND:123596"
  "DRUGBANK:DB00619")

(define imatinib-regulates
  (query:Known->X
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db (list "PUBCHEM.COMPOUND:5291"))))
   (set->list
    (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
     '("biolink:regulates")))
   #f))
