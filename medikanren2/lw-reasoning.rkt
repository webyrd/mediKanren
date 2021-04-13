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

(define-relation (inverse-of p p^)
  (conde ((triple p "biolink:inverse_of" p^))
         ((triple p^ "biolink:inverse_of" p))))

(define-relation (triple/inverse s p o)
  (conde ((triple s p o))
         ((fresh (p^) 
            (inverse-of p p^)
            (triple o p^ s)))))

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

(define-relation (edge-predicate/subclass eid p p^)
  (conde ((eprop eid "predicate" p))
         ((subclass-of+ p^ p)
          (eprop eid "predicate" p^))))

(define-relation (is-a/subclass* s c c^)
  (is-a s c^)
  (subclass-of* c^ c))

(define-relation (is-a/subclass+ s c c^)
  (subclass-of+ c^ c)
  (is-a s c^))

;; for testing only; not a useful relation as far as I can see
(define-relation (triple/subclass+ s p p^ o)
  (subclass-of+ p^ p)
  (triple/inverse s p^ o))

(define-relation (triple/subclass s p p^ o)
  (conde ((triple s p o) (== p p^))
         ((triple/subclass+ s p p^ o))))

(define-relation (triple/reasoning s s^ p p^ o o^)
  (subclass-of* s^ s)
  (subclass-of* p^ p)
  (subclass-of* o^ o)
  (triple s^ p^ o^))

(define-relation (triple/reasoning+ s s^ p p^ o o^)
  (triple s^ p^ o^)
  (subclass-of+ s^ s)
  (subclass-of+ p^ p)
  (subclass-of+ o^ o)
  )

(define lw-reasoning? (make-parameter #f))

(define-relation (is-a/lwr s c)
  (if (lw-reasoning?)
      (fresh (c^)
        (is-a/subclass* s c c^))
      (is-a s c)))

(define-relation (triple/lwr s p o)
  (if (lw-reasoning?)
      (fresh (p^) 
        (triple/subclass s p p^ o))
      (triple s p o)))

(define-relation (edge-predicate/lwr eid p)
  (if (lw-reasoning?)
      (fresh (p^)
        (conde ((eprop eid "predicate" p))
               ((subclass-of+ p^ p)
                (eprop eid "predicate" p^))))
      (eprop eid "predicate" p)))





