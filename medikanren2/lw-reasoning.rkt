#lang racket/base
(provide (all-from-out "synonyms.rkt")
         lw-reasoning?
         inverse-of triple/inverse edge-predicate/lwr
         subclass-of subclass-of* subclass-of+
         is-a/subclass+ is-a/subclass*
         is-a/lwr triple/lwr triple/inverse
         triple/subclass triple/subclass+
         subclasses/set
         )
(require
 "common.rkt"
 "synonyms.rkt"
  racket/file racket/function racket/list racket/hash
  (except-in racket/match ==)
  racket/port
  racket/pretty
  racket/runtime-path
  racket/string
  racket/set
  json
  memoize
  )

;; Generalized transitive closure

(define (transitive-closure base-relation)
  (lambda (child parent)
    (cond ((string? child)
           ((transitive-closure/up base-relation) child parent))
          ((string? parent)
           ((transitive-closure/down base-relation) child parent))
          (else ((transitive-closure/mid base-relation) child parent)))))

;; ideally this would be the only relation, but currently the other two optimizations are needed
(define (transitive-closure/mid base-relation)
  (define-relation (~transitive-closure/mid child parent)
    (conde ((base-relation child parent))
           ((fresh (mid)
              (base-relation child mid)
              (base-relation mid parent)))
           ((fresh (mid1 mid2)
              (base-relation child mid1)
              (~transitive-closure/mid mid1 mid2)
              (base-relation mid2 parent)))))
  ~transitive-closure/mid)

(define (transitive-closure/down base-relation)
  (define-relation (~transitive-closure/down child parent)
    (fresh (mids)
      (:== mids (parent) (run* mid (base-relation mid parent)))
      (fresh (mid)
        (membero mid mids)
        (conde ((== child mid))
               ((~transitive-closure/down child mid))))))
  ~transitive-closure/down)

;; (define (transitive-closure/down^ base-relation)
;;   (define-relation (~transitive-closure/down^ child parent)
;;     (conde ((base-relation child parent))
;;            ((fresh (mid)
;;             (base-relation mid parent)
;;             (~transitive-closure/down^ child mid)))))
;;   ~transitive-closure/down^)

(define (transitive-closure/up base-relation)
  (define-relation (~transitive-closure/up child parent)
    (fresh (mids)
      (:== mids (child) (run* mid (base-relation child mid)))
      (fresh (mid)
        (membero mid mids)
        (conde ((== parent mid))
               ((~transitive-closure/up mid parent))))))
  ~transitive-closure/up)

;; inverse predicates
    
(define-relation (inverse-of p p^)
  (conde ((triple p "biolink:inverse_of" p^))
         ((triple p^ "biolink:inverse_of" p))))

(define-relation (triple/inverse s p o)
  (conde ((triple s p o))
         ((fresh (p^) 
            (inverse-of p p^)
            (triple o p^ s)))))

;; subclass relations

(define-relation (subclass-of child parent)
  (triple child "biolink:subclass_of" parent))

(define subclass-of+ (transitive-closure subclass-of))

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

(define/memo* (subclasses/set-of curie)
  (run* cc (subclass-of* cc curie)))

(define/memo* (subclasses/set curies)
  (run* c (fresh (cs c^)
            (membero c^ curies)
            (:== cs (c^) (subclasses/set-of c^))
            (membero c cs))))

;; Attempt to do synonymization and subclasses at once 

(define-relation (subclass-or-synonym a b)
  (conde ((direct-synonym a b))
         ((direct-synonym b a))
         ((subclass-of a b))))

(define-relation (subclass-or-synonym/mid a b)
  (conde ((subclass-or-synonym a b))
         ((fresh (mid)
            (subclass-or-synonym a mid)
            (subclass-or-synonym mid b)))
         ((fresh (mid1 mid2)
            (subclass-or-synonym a mid1)
            (subclass-or-synonym mid2 b)
            (subclass-or-synonym/mid mid1 mid2)))))

(define-relation (subclass-or-synonym/down a b)
  (conde ((subclass-or-synonym a b))
         ((fresh (mid)
            (subclass-or-synonym mid b)
            (subclass-or-synonym/down a mid)))))

(define-relation (subclass-or-synonym/up a b)
  (conde ((subclass-or-synonym a b))
         ((fresh (mid)
            (subclass-or-synonym a mid)
            (conde ((subclass-or-synonym mid b))
                   ((subclass-or-synonym/up mid b)))))))
         


         
;; (define anxiety "HP:0000739")
;; (time (display (length (set->list (synonyms/breadth anxiety 1)))))
;; (time (display (length (set->list (synonyms/breadth anxiety 2)))))
;; (time (display (length (set->list (synonyms/breadth anxiety 3)))))

;; (time (display (length (set->list (synonyms/step anxiety 200)))))
;; (time (display (length (set->list (synonyms/step anxiety 1000)))))
;; (time (display (length (set->list (synonyms/step anxiety 10000)))))
;; (define (run-test n)
;;   (time
;;    (display
;;     (length
;;      (set->list (run*/set/steps n s (subclass/synonym-of s anxiety)))))))
;; (run-test 200)
;; (run-test 1000)
;; (run-test 10000)



;; todo: try to make a version that terminates
;; > (time (set-count (run*/set/steps 10000 (s path) (subclass/synonym-of s anxiety path))))
;; cpu time: 21026 real time: 21032 gc time: 122
;; 55
;; >  (time (set-count (run*/set/steps 15000 (s path) (subclass/synonym-of s anxiety path))))
;; cpu time: 32172 real time: 32184 gc time: 205
;; 71
;; >  (time (set-count (run*/set/steps 20000 (s path) (subclass/synonym-of s anxiety path))))
;; cpu time: 45528 real time: 45947 gc time: 378
;; 71
