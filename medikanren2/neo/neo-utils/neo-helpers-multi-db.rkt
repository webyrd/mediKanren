#lang racket/base

(provide
 curie->synonyms-in-db
 curies->synonyms-in-db
 ;;
 all-predicates-in-db
 all-classes-in-db
 ;;
 get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
 get-non-deprecated-mixed-ins-and-descendent-classes*-in-db
 ;;
 get-descendent-curies-in-db
 get-descendent-curies*-in-db
 ;;
 iota
 pretty-print-json-string
 take-at-most
 )
(require
 "../neo-low-level/query-low-level-multi-db.rkt"
 "../neo-reasoning/neo-biolink-reasoning.rkt"
 racket/set
 "neo-helpers-without-db.rkt")

(define (curie->synonyms-in-db curie)
  (filter curie-in-db? (curie->synonyms curie)))

(define (curies->synonyms-in-db curies)
  (filter curie-in-db? (curies->synonyms curies)))

(define all-predicates-in-db
  (list->set
    (filter curie-in-db?
            (set->list all-predicates))))
(define all-classes-in-db
  (list->set
    (filter curie-in-db?
            (set->list all-classes))))

(define (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db preds)
  (list->set
    (filter curie-in-db?
            (set->list
              (get-non-deprecated-mixed-ins-and-descendent-predicates* preds)))))

(define (get-non-deprecated-mixed-ins-and-descendent-classes*-in-db classes)
  (list->set
    (filter curie-in-db?
            (set->list
              (get-non-deprecated-mixed-ins-and-descendent-classes* classes)))))

(define (get-descendent-curies-in-db curie)
  (get-descendent-curies*-in-db (list curie)))

(define (get-descendent-curies*-in-db curies)
  (set-union (list->set curies)
   (set-fixed-point
    (list->set
     (map car
          (query:X->Known
           #f
           (list "biolink:subclass_of")
           curies)))
    (lambda (new-curies)
      (list->set
       (map car
            (query:X->Known
             #f
             (list "biolink:subclass_of")
             (set->list new-curies))))))))
