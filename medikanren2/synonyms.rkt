#lang racket/base
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
  memoize)

(provide direct-synonym direct-synonym* direct-synonym+ synonym close-match-synonym
         synonyms/step synonym-of/step synonyms/breadth synonym-of/breadth
         synonym synonym-path kgx-synonym
         simple-synonym
         get-synonyms-ls get-names-ls get-names-set)

(define (equivalence-relation node link)
  (define-relation (equiv=/= a b)
    (=/= a b)
    (fresh (x)
      (conde ((link a x))
             ((link x a)))
      (conde ((==       x b))
             ((equiv=/= x b)))))
  (define-relation (equiv a b)
    (conde ((==       a b) (node a) (node b))
           ((equiv=/= a b))))
  equiv)

(define (simple-node x) (fresh (k v) (cprop x k v)))

(define (simple-link a b)
  (fresh (id pred)
    (edge a b)
    (eprop id "predicate" pred)
    (membero pred '("biolink:same_as"))))

(define simple-synonym (equivalence-relation simple-node simple-link))


(define synonyms-preds '("biolink:same_as"
                         ;; "biolink:close_match"   ; too risky un-constrained
                         "biolink:has_gene_product"))

(define rtx2-drug-categories '("biolink:ChemicalSubstance"
                               "biolink:ClinicalIntervention"
                               "biolink:ClinicalModifier"
                               "biolink:Drug"
                               "biolink:Treatment"))

(define semmed-drug-categories '("chemical_substance"))

(define drug-categories (append rtx2-drug-categories semmed-drug-categories))

(define drug-categories/set (list->set drug-categories))

(define disease-categories '("biolink:Disease"
                             "biolink:DiseaseOrPhenotypicFeature"
                             "biolink:PhenotypicFeature"))

(define disease-categories/set (list->set disease-categories))

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

(define gene-or-protein/set (list->set gene-or-protein))

; Cached synonyms from the kgx-synonym knowledge graph
(define (kgx-synonym a b)
  (fresh (predicate id source_database)
    (conde
      [(kgx:synonym a b predicate id source_database)]
      [(kgx:synonym b a predicate id source_database)])))

;; Query synonyms from loaded graphs
(define-relation (direct-synonym a b)
  (fresh (id sp)
    (edge id a b)
    (eprop id "predicate" sp)
    (membero sp synonyms-preds)))

;; More constrained approach
;; (define-relation (close-match-syn a b)
;;   (fresh (id)
;;     (any<=o "HGNC:" a)
;;     (any<=o a "HGND")
;;     (any<=o "UMLS:" b)
;;     (any<=o b "UMLT")
;;     (edge id a b)
;;     (:== #t (a) (not (null? (run 1 () (eprop id "predicate" "biolink:close_match")))))
;;     (:== #t (a) (string-prefix? a "HGNC:"))
;;     (:== #t (b) (string-prefix? b "UMLS:"))))

(define (non-empty-intersection/ls ls1 ls2)
  (not (set-empty? (set-intersect (list->set ls1) (list->set ls2)))))

(define (non-empty-intersection set1 set2)
  (not (set-empty? (set-intersect set1 set2))))

(define-relation (close-match-synonym a b)
  (fresh (id cat-a cat-b)
    (edge id a b)
    (eprop id "predicate" "biolink:close_match")
    (:== cat-a (a) (run*/set c (is-a a c)))
    (:== cat-b (b) (run*/set c (is-a b c)))
    (conde ((:== #t (cat-a) (non-empty-intersection cat-a disease-categories/set) )
            (:== #t (cat-b) (non-empty-intersection cat-b disease-categories/set)))
           ((:== #t (cat-a) (non-empty-intersection cat-a drug-categories/set) )
            (:== #t (cat-b) (non-empty-intersection cat-b drug-categories/set)))
           ((:== #t (cat-a) (non-empty-intersection cat-a gene-or-protein/set))
            (:== #t (cat-b) (non-empty-intersection cat-b gene-or-protein/set))
            (conde ((:== #t (a) (string-prefix? a "HGNC:")))
                   ((:== #t (a) (string-prefix? a "NCBIGene:")))
                   ((:== #t (a) (string-prefix? a "UniProtKB:")))
                   ((:== #t (a) (string-prefix? a "ENSEMBL:"))))))))

(define-relation (direct-synonym* a b)
  (conde ((== a b))
         ((direct-synonym+ a b))))

(define-relation (direct-synonym+ a b)
  (conde ((direct-synonym a b))
         ((fresh (mid)
            (direct-synonym a mid)
            (direct-synonym+ mid b)))))

(define-relation (exact-synonym a b)
  (conde ((== a b))
         ((direct-synonym+ a b))
         ((direct-synonym+ b a))))

(define-relation (synonym a b)
  (conde ((== a b))
         ((close-match-synonym a b))
         ((close-match-synonym b a))
         ((direct-synonym a b))
         ((direct-synonym b a))
         ((fresh (mid)
            (conde ((close-match-synonym a mid)) ((close-match-synonym mid a)))
            (synonym mid b)))
         ((fresh (mid)
            (conde ((close-match-synonym b mid)) ((close-match-synonym mid b)))
            (synonym mid a)))         
         ((fresh (mid)
            (conde ((direct-synonym a mid)) ((direct-synonym mid a)))
            (synonym mid b)))
         ((fresh (mid)
            (conde ((direct-synonym b mid)) ((direct-synonym mid b)))
            (synonym mid a)))))

(define-relation (synonym-path a b path)
  (conde
    ((== a b) (== path `(,a)))
    ((direct-synonym a b) (== path `(,a = ,b)))
    ((direct-synonym b a) (== path `(,a = ,b)))
    ((close-match-synonym a b) (== path `(,a ~ ,b)))
    ((close-match-synonym b a) (== path `(,a ~ ,b)))
    ((fresh (mid path-rest)
       (conde      
         ((conde ((direct-synonym a mid)) ((direct-synonym mid a)))
          (synonym-path mid b path-rest)
          (not-membero a path-rest)
          (== path `(,a = . ,path-rest)))
         ((conde ((direct-synonym b mid)) ((direct-synonym mid b)))
          (synonym-path a mid path-rest)
          (not-membero b path-rest)
          (appendo path-rest (list '= b) path))
         ((conde ((close-match-synonym a mid)) ((close-match-synonym mid a)))
          (synonym-path mid b path-rest)
          (not-membero a path-rest)
          (== path `(,a ~ . ,path-rest)))
         ((conde ((close-match-synonym b mid)) ((close-match-synonym mid b)))
          (synonym-path a mid path-rest)
          (not-membero b path-rest)
          (appendo path-rest (list '~ b) path)))))))

;; Different ways of actually getting a useful amount of synonyms in a reasonable amount of time

(define (synonyms/step term (n 500) (categories '()))
  (if (pair? categories)
      (set->list
       (run*/set/steps n s
         (synonym s term)
         (fresh (cat)
           (cprop s "category" cat)
           (membero cat categories))))
      (set->list (run*/set/steps n s (synonym s term)))))

;; relation for use in mk queries
;; n is required
;; (run* s ((synonym-of/set autism 1000) s)))
(define/memo* (synonym-of/step term n)
  (let ((synonyms (set->list (synonyms/step term n))))
    (relation synonym-of/step^ (s)
      (membero s synonyms))))

(define (synonyms/breadth term (n 2) (categories '()))
  (let loop ((n (- n 1)) (synonyms (set term)) (terms (list term)) )
    (let ((new-synonyms
           (run*/set s (fresh (term)
                         (membero term terms)
                         (conde ((direct-synonym s term))
                                ((direct-synonym term s))
                                ((close-match-synonym s term))
                                ((close-match-synonym term s)))
                         (if (pair? categories)
                             (fresh (cat)
                               (cprop s "category" cat)
                               (membero cat categories))
                             (== #t #t))
                         (:== #f (s) (set-member? synonyms s))
                         ;; (not-membero s (set->list synonyms))     ; purer but slower
                         ))))
      (cond ((set-empty? new-synonyms) (set->list synonyms))
            ((= n 0) (set->list (set-union new-synonyms synonyms)))
            (else
             (loop (- n 1) (set-union new-synonyms synonyms) 
                   (set->list new-synonyms)))))))

;; relation for use in mk queries
;; n is required
;; (run* s ((synonym-of/breadth autism 2) s)))
(define/memo* (synonym-of/breadth term n)
  (let ((synonyms (synonyms/breadth term n)))
    (relation synonym-of/breadth^ (s)
      (membero s synonyms))))

;; used in TRAPI interpreter
;; implements basic logic: KGX-syn for genes/proteins, and KGs for others
(define/memo* (get-synonyms-ls curies)
  (remove-duplicates
   (append curies
           (apply append
                  (map (lambda (curie)
                         (let ((cats (run*/set c (cprop curie "category" c))))
                           (if (non-empty-intersection cats gene-or-protein/set)
                               (synonyms/breadth curie 1)
                               (run* s (kgx-synonym curie s)))))
                       curies)))))

; get-names-ls function takes a list of curies and return the curies with their coresponding names
(define (get-names-ls curie-ls)
  (run* (curie name)
    (cprop curie "name" name)
    (membero curie curie-ls)))

(define (get-names-set curie-set)
  (get-names-ls (set->list curie-set)))


  
