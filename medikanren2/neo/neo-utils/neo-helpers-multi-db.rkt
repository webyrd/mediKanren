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
 ;;
 auto-grow
 auto-grow-with-class-hierarchy
 )
(require
 "../neo-low-level/query-low-level-multi-db.rkt"
 "../neo-reasoning/neo-biolink-reasoning.rkt"
 racket/set
 "neo-helpers-without-db.rkt"
 racket/list
 racket/string)

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

(define (curie-is-type? curie type)
  (let* ((node-prop (curie->properties curie))
        (umls-description (assoc "description" node-prop)))
    (and umls-description (string-contains? (cadr umls-description) type))))

(define (edge-from-source edge source^)
  (let* ((props (cdddr edge))
         (source (assoc "primary_knowledge_source" props))
         (source (and source (cadr source))))
    (and source (equal? source source^))))

(define (get-n-descendent-curies*-in-db curies n)
  (list->set
   (append curies
           (let loop ((r '()) (c curies))
             (if (or (> (length r) n) (= (length r) n))
                 r
                 (let* ((children (remove-duplicates
                                   (map car
                                        (filter
                                         (lambda (e) (not (edge-from-source e "infores:medrt-umls")))
                                         (query:X->Known-scored
                                          #f
                                          (list "biolink:subclass_of")
                                          c
                                          (list (list 1111) #f (list 1111)))))))
                        (not-classification-type (filter (lambda (c) (not (curie-is-type? c "STY:T185"))) children))
                        (new-r (remove-duplicates (append r not-classification-type))))
                   (if (= (length r) (length new-r))
                       r
                       (loop new-r not-classification-type))))))))

(define (get-next-descendent-curies* curies)
  (let* ((children (remove-duplicates
                    (map car
                         (filter
                          (lambda (e) (not (edge-from-source e "infores:medrt-umls")))
                          (query:X->Known-scored
                           #f
                           (list "biolink:subclass_of")
                           curies
                           (list (list 1111) #f (list 1111)))))))
         (not-classification-type-children (filter (lambda (c) (not (curie-is-type? c "STY:T185"))) children)))
    not-classification-type-children))

(define (auto-grow-with-class-hierarchy 1hop? proc-template score* self-curie* result-amount)
  (let loop ((r '()) (hierarchy 1) (desired-size result-amount) (c* self-curie*) (seen-curies '()))
    (cond
      [(or (< desired-size 0) (= desired-size 0)) r]
      [(null? c*) r]
      [else
       (let* ((proc (proc-template c*))
              (new-results (if 1hop? (auto-grow proc score* desired-size) (proc score* desired-size)))
              (new-results-with-hierarchy-property (map (lambda (r) (cons hierarchy r)) new-results))
              (new-results-size (length new-results-with-hierarchy-property))
              (next-descendent (get-next-descendent-curies* c*))
              (next-descendent (filter (lambda (c) (not (member c seen-curies))) next-descendent)))
         (loop (append r new-results-with-hierarchy-property)
               (+ hierarchy 1)
               (- desired-size new-results-size)
               next-descendent
               (append seen-curies next-descendent)
               ))])))
