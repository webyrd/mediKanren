#lang racket/base
(provide lw-reasoning?
         inverse-of triple/inverse edge-predicate/lwr
         subclass-of subclass-of* subclass-of+
         is-a/lwr triple/lwr triple/inverse)
(require
  "common.rkt"
  racket/file racket/function racket/list racket/hash
  (except-in racket/match ==)
  racket/port
  racket/pretty
  racket/runtime-path
  racket/string
  json
  )

(define lw-reasoning? (make-parameter #f))

(define-relation (inverse-of p r)
  (conde ((triple p "biolink:inverse_of" r))
         ((triple r "biolink:inverse_of" p))))

(define-relation (triple/inverse s p o)
  (conde ((triple s p o))
         ((fresh (p*) 
            (inverse-of p p*)
            (triple o p* s)))))

(define-relation (subclass-of child parent)
  (triple child "biolink:subclass_of" parent))

(define-relation (subclass-of+ child parent)
  (conde ((subclass-of child parent))
         ((fresh (mid)
            (subclass-of child mid)
            (subclass-of mid parent)))
         ((fresh (mid1 mid2)
            (subclass-of child mid1)
            (subclass-of mid2 parent)
            (subclass-of+ mid1 mid2)))))

(define-relation (subclass-of* child parent)
  (conde ((== child parent))
         ((subclass-of+ child parent))))

(define-relation (edge-predicate/lwr eid p)
  (if (lw-reasoning?) 
      (conde ((eprop eid "predicate" p))
             ((fresh (p*)
                (conde ((inverse-of p* p))
                       ((subclass-of+ p* p)))
                (eprop eid "predicate" p*))))
      (eprop eid "predicate" p)))

(define-relation (concept/subclass+ child parent)
  (fresh (cc pc)
    (is-a child cc) 
    (is-a parent pc)
    (subclass-of+ cc pc)))

(define-relation (concept/subclass* child parent)
  (conde ((== child parent))
         ((concept/subclass+ child parent))))

(define-relation (is-a/subclass s c)
  (fresh (c2)
    (subclass-of c2 c)
    (is-a s c2)))

(define-relation (is-a/subclass* s c)
  (fresh (c2)
    (subclass-of* c2 c)
    (is-a s c2)))

(define-relation (is-a/subclass+ s c)
  (fresh (c2)
    (subclass-of+ c2 c)
    (is-a s c2)))

(define-relation (is-a/lwr s c)
  (if (lw-reasoning?)
      (is-a/subclass* s c)
      (is-a s c)))

;; (define-relation (triple/lwr s p o)
;;   (if (lw-reasoning?)
;;       (conde ((triple s p o))
;;              ((fresh (s* p* o*)
;;                 (concept/subclass* s* s)
;;                 (concept/subclass* o* o)
;;                 (subclass-of* p* p)
;;                 (triple/inverse s* p* o*))))
;;       (triple s p o)))


(define-relation (triple/lwr s p o)
  (if (lw-reasoning?)
      (conde ((triple s p o))
             ((fresh (p*)
                (subclass-of+ p* p)
                (triple/inverse s p* o))))
      (triple s p o)))

(define-relation (triple/lwr+ s p o)
  (fresh (p*)
    (subclass-of+ p* p)
    (triple s p* o)))

(define-relation (triple/so+ s p o)
  (fresh (s* o*)
    (concept/subclass+ s* s)
    (concept/subclass+ o* o)
    (triple s* p o*)))





