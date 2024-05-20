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
 get-non-deprecated/mixin/abstract-ins-and-descendent-predicates*-in-db
 get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
 ;;
 get-descendent-curies-in-db
 get-descendent-curies*-in-db
 get-n-descendent-curies*-in-db
 ;;
 iota
 pretty-print-json-string
 take-at-most
 )
(require
 "../neo-low-level/query-low-level-multi-db.rkt"
 "../neo-reasoning/neo-biolink-reasoning.rkt"
 racket/set
 "neo-helpers-without-db.rkt"
 racket/list)

(define (curie->synonyms-in-db curie)
  (curies-in-db (curie->synonyms curie)))

(define (curies->synonyms-in-db curies)
  (curies-in-db (curies->synonyms curies)))

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
   (curies-in-db
    (set->list
     (get-non-deprecated-mixed-ins-and-descendent-predicates* preds)))))

(define (get-non-deprecated/mixin/abstract-ins-and-descendent-predicates*-in-db preds)
  (list->set
   (filter (lambda (p)
             (not (or
                   (predicate-mixin? p)
                   (predicate-abstract? p))))
           (set->list (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db preds)))))

(define (get-non-deprecated-mixed-ins-and-descendent-classes*-in-db classes)
  (list->set
    (curies-in-db
            (set->list
              (get-non-deprecated-mixed-ins-and-descendent-classes* classes)))))

(define (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db classes)
  (list->set
   (filter (lambda (c)
             (not (or
                   (class-mixin? c)
                   (class-abstract? c))))
           (set->list (get-non-deprecated-mixed-ins-and-descendent-classes*-in-db classes)))))

(define (get-descendent-curies-in-db curie)
  (get-descendent-curies*-in-db (list curie)))

(define (get-descendent-curies*-in-db curies)
  (set-union (list->set curies)
   (set-fixed-point
    (list->set
     (map car
          (query:X->Known-scored
           #f
           (list "biolink:subclass_of")
           curies
           (list (list 1111) #f (list 1111)))))
    (lambda (new-curies)
      (list->set
       (map car
            (query:X->Known-scored
             #f
             (list "biolink:subclass_of")
             (set->list new-curies)
             (list (list 1111) #f (list 1111)))))))))

(define (get-n-descendent-curies*-in-db curies n)
  (list->set
   (append curies
           (let loop ((r '()) (c curies))
             (if (or (> (length r) n) (= (length r) n))
                 r
                 (let* ((children (map car
                                       (query:X->Known-scored
                                        #f
                                        (list "biolink:subclass_of")
                                        c
                                        (list (list 1111) #f (list 1111)))))
                        (new-r (remove-duplicates (append r children))))
                   (if (= (length r) (length new-r))
                       r
                       (loop new-r children))))))))
             
    