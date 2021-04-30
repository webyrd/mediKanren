#lang racket/base
(provide lw-reasoning?
         inverse-of triple/inverse edge-predicate/lwr
         subclass-of subclass-of* subclass-of+
         is-a/subclass+ is-a/subclass*
         is-a/lwr triple/lwr triple/inverse
         triple/subclass triple/subclass+
         direct-synonym direct-synonym* direct-synonym+ synonym
         synonym-of/step synonym-of/breadth
         syns/set synonyms/set subclasses/set)
(require
 (except-in "common.rkt" synonym)
  racket/file racket/function racket/list racket/hash
  (except-in racket/match ==)
  racket/port
  racket/pretty
  racket/runtime-path
  racket/string
  racket/set
  json
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

;; Synonymization 1: Cached synonyms

(define (syns/set curies)
  (append curies
          (run* s (fresh (curie)
                    (membero curie curies)
                    (syn curie s)))))


;; Synonymization 2: queried synonyms

(define synonyms-preds '("biolink:same_as"
                         "biolink:close_match"
                         "biolink:has_gene_product"))

(define synonyms-exact-preds '("biolink:same_as"))

(define rtx2-drug-categories '("biolink:ChemicalSubstance"
                               "biolink:ClinicalIntervention"
                               "biolink:ClinicalModifier"
                               "biolink:Drug"
                               "biolink:Treatment"))

(define semmed-drug-categories '("chemical_substance"))

(define drug-categories (append rtx2-drug-categories semmed-drug-categories))

(define disease-categories '("biolink:Disease"
                             "biolink:DiseaseOrPhenotypicFeature"
                             "biolink:PhenotypicFeature"))

(define inhibit-preds '("biolink:decreases_activity_of"
                        "biolink:decreases_expression_of"
                        "biolink:disrupts"
                        "biolink:negatively_regulates"
                        "biolink:negatively_regulates,_entity_to_entity"
                        "biolink:negatively_regulates,_process_to_process"
                        "biolink:treats"
                        "negatively_regulates" ; semmed
                        "treats" ; semmed
                        )) 

(define gene-or-protein '("biolink:Gene"
                          "biolink:GeneFamily"
                          "biolink:GeneProduct"
                          "biolink:GenomicEntity"
                          "biolink:MacromolecularComplex"
                          "biolink:MolecularEntity"
                          "biolink:Protein"))

(define-relation (direct-synonym a b)
  (fresh (id sp)
    (rtx:edge id a b)
    (rtx:eprop id "predicate" sp)
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

(define (synonyms/step term (n 200) (categories '()))
  (if (pair? categories)
      (set->list
       (run*/set/steps n s
         (synonym s term)
         (fresh (cat)
           (cprop s "category" cat)
           (membero cat categories))))
      (set->list (run*/set/steps n s (synonym s term)))))

(define (synonyms/breadth term (n 2) (categories '()))
  (let loop ((n (- n 1)) (synonyms (set term)) (terms (list term)) )
    (let ((new-synonyms
           (run*/set s (fresh (term)
                         (conde ((direct-synonym s term))
                                ((direct-synonym term s)))
                         (membero term terms)
                         (if (pair? categories)
                             (fresh (cat)
                               (cprop s "category" cat)
                               (membero cat categories))
                             (== #t #t))
                         (:== #f (s) (set-member? synonyms s))
                         ;; (not-membero s (set->list synonyms))     ; purer but slower
                         ))))
      (cond ((set-empty? new-synonyms) synonyms)
            ((= n 0) (set-union new-synonyms synonyms))
            (else
             (loop (- n 1) (set-union new-synonyms synonyms) 
                   (set->list new-synonyms)))))))
          
(define (synonym-of/step term (n 200))
  (relation synonym-of/step^ (s)
    (fresh (synonyms)
      (:== synonyms (term) (synonyms/step term n))
      (membero s synonyms))))

(define (synonym-of/breadth term (n 2))
  (relation synonym-of/breadth^ (s)
    (fresh (synonyms)
      (:== synonyms (term) (set->list (synonyms/breadth term n)))
      (membero s synonyms))))

 (define (subclasses/set curies)
  (run* c (fresh (cs c^)
            (membero c^ curies)
            (:== cs (c^) (run* cc (subclass-of* cc c^)))
            (membero c cs))))

(define (synonyms/set curies)
  (run* s (fresh (curie)
            (membero curie curies)
            ((synonym-of/step curie) s))))

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
