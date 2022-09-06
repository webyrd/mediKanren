#lang racket/base

(provide
 curie->synonyms-in-db
 curies->synonyms-in-db
 ;;
 all-predicates-in-db
 all-classes-in-db
 ;;
 get-predicate-descendents-in-db
 get-class-descendents-in-db
 ;;
 get-predicate-descendents*-in-db
 get-class-descendents*-in-db
 ;;
 get-descendent-curies-in-db
 get-descendent-curies*-in-db
 )
(require
 "../dbKanren/dbk/database.rkt"
 "../neo-low-level/query-low-level.rkt"
 "../neo-low-level/synonym-low-level.rkt"
 "../neo-reasoning/neo-biolink-reasoning.rkt"
 racket/set)

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

(define (get-predicate-descendents-in-db pred)
  (list->set
    (filter curie-in-db?
            (set->list (get-predicate-descendents pred)))))
(define (get-class-descendents-in-db class)
  (list->set
    (filter curie-in-db?
            (set->list (get-class-descendents class)))))

(define (get-predicate-descendents*-in-db preds)
  (list->set
    (filter curie-in-db?
            (set->list (get-predicate-descendents* preds)))))
(define (get-class-descendents*-in-db classes)
  (list->set
    (filter curie-in-db?
            (set->list (get-class-descendents*-in-db classes)))))


(define (get-descendent-curies-in-db curie)
  (get-descendent-curies*-in-db (list curie)))

(define (get-descendent-curies*-in-db curies)
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
            (set->list new-curies)))))))
