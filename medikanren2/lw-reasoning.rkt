#lang racket/base
(provide lw-reasoning?
         inverse-of triple/inverse edge-predicate/lwr
         subclass-of subclass-of* subclass-of+
         is-a/subclass+ is-a/subclass*
         is-a/lwr triple/lwr triple/inverse
         synonym-of direct-synonym direct-synonym* direct-synonym+ synonym
         synonyms/set subclasses/set)
(require
  "common.rkt"
  racket/file racket/function racket/list racket/hash
  (except-in racket/match ==)
  racket/port
  racket/pretty
  racket/runtime-path
  racket/string
  racket/set
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

;; the 'pure' subclass-of+ relation
(define-relation (subclass-of/mid child parent)
  (conde ((subclass-of child parent))
         ((fresh (mid)
            (subclass-of child mid)
            (subclass-of mid parent)))
         ((fresh (mid1 mid2)
           (subclass-of child mid1)
           (subclass-of+ mid1 mid2)
           (subclass-of mid2 parent)))))

(define (subclass-of+ child parent)
  (cond ((string? child)
         (subclass-of/up child parent))
        ((string? parent)
         (subclass-of/down child parent))
        (else (subclass-of/mid child parent))))

(define-relation (subclass-of/down child parent)
  (fresh (mids)
    (:== mids (parent) (run* mid (subclass-of mid parent)))
    (fresh (mid)
      (membero mid mids)
      (conde ((== child mid))
             ((subclass-of/down child mid))))))

(define-relation (subclass-of/up child parent)
  (fresh (mids)
    (:== mids (child) (run* mid (subclass-of child mid)))
    (fresh (mid)
      (membero mid mids)
      (conde ((== parent mid))
             ((subclass-of/up mid parent))))))

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
  (subclass-of+ o^ o))

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

;; Synonyms

(define synonyms-preds '("biolink:same_as"
                         "biolink:close_match"
                         "biolink:has_gene_product"))

;; (define synonyms-preds '("biolink:same_as"))

(define-relation (direct-synonym a b)
  (fresh (id sp)
    (edge id a b)
    (eprop id "predicate" sp)
    (membero sp synonyms-preds)))
(define-relation (direct-synonym* a b)
  (conde ((== a b))
         ((direct-synonym+ a b))))
(define-relation (direct-synonym+ a b)
  (conde ((direct-synonym a b))
         ((fresh (mid)
            (direct-synonym a mid)
            (direct-synonym+ mid b)))))
(define-relation (synonym a b)
  (conde ((== a b))
         ((direct-synonym+ a b))
         ((direct-synonym+ b a))))
(define (synonyms/step term (n 200))
  (set->list (run*/set/steps n s (synonym s term))))
(define (synonym-of term (n 200))
  (relation synonym-of^ (s)
    (fresh (synonyms)
      (:== synonyms (term) (synonyms/step term n))
      (membero s synonyms))))

;; slower than the above for fewer synonyms; but sometimes faster for more synonyms 
(define (synonyms/breadth a (n -1))
  (let loop ((n (- n 1)) (synonyms (set a)) (terms (list a)) )
    (let ((new-synonyms
           (run*/set s (fresh (term)
                         (conde ((direct-synonym s term))
                                ((direct-synonym term s)))
                         (membero term terms)
                         (:== #f (s) (set-member? synonyms s))
                         ;; (not-membero s (set->list synonyms))
                         ))))
      (let ((diff (set-intersect new-synonyms synonyms)))
        (when (not (set-empty? diff))
          (error "error: " diff)))
      (cond ((set-empty? new-synonyms) synonyms)
            ((= n 0) (set-union new-synonyms synonyms))
            (else
             (loop (- n 1) (set-union new-synonyms synonyms) 
                   (set->list new-synonyms)))))))
          
 (define (subclasses/set curies)
  (run* c (fresh (cs c^)
            (membero c^ curies)
            (:== cs (c^) (run* cc (subclass-of* cc c^)))
            (membero c cs))))

(define (synonyms/set curies)
  (run* s (fresh (curie)
            (membero curie curies)
            ((synonym-of curie) s))))
