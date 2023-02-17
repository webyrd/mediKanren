#lang racket/base
(provide
 ;;
 get-biolink-version
 ;;
 class-deprecated?
 predicate-mixin?
 class-mixin?
 predicate-symmetric?
 ;;
 all-predicates
 all-classes
 ;;
 get-inverse-predicate
 ;;
 get-non-deprecated-mixed-ins-and-descendent-predicates*
 ;;
 get-non-deprecated-mixed-ins-and-descendent-classes*
 ;;
 )
(require
 racket/set
 "../neo-utils/neo-helpers-without-db.rkt"
 "neo-biolink-reasoning-low-level.rkt")

;; Return non-deprecated descendent predicates and mixins, and
;; non-deprecated predicates that include any of those mixins, from a
;; given list of predicates and/or mixins.
(define get-non-deprecated-mixed-ins-and-descendent-predicates*
  (lambda (predicate*)
    (list->set
     (filter (lambda (c) (not (predicate-deprecated? c)))
             (set->list (get-mixed-ins-and-descendent-predicates* predicate*))))))

;; Return non-deprecated descendent classes and mixins, and
;; non-deprecated classes that include any of those mixins, from a
;; given list of classes and/or mixins.
(define get-non-deprecated-mixed-ins-and-descendent-classes*
  (lambda (class*)
    (list->set
     (filter (lambda (c) (not (class-deprecated? c)))
             (set->list (get-mixed-ins-and-descendent-classes* class*))))))
