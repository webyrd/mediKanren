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

(provide direct-synonym direct-synonym* direct-synonym+ synonym
         synonym-of/step synonym-of/breadth
         synonyms/set synonyms/breadth
         synonym kgx-synonym
         get-synonyms get-synonyms-ls
         synonym-of)

(define synonyms-preds '("biolink:same_as"
                         ;; "biolink:close_match"
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



(define/memo* (get-cat curie) (car (run 1 cat (cprop curie "category" cat))))
(define-relation (close-match-synonym a b)
  (fresh (id )
    (edge id a b)
    (eprop id "predicate" "biolink:close_match")
    (conde ((:== #t (a) (pair? (member (get-cat a) disease-categories) ))
            (:== #t (b) (pair? (member (get-cat b) disease-categories) )))
           ((:== #t (a) (pair? (member (get-cat a) drug-categories) ))
            (:== #t (b) (pair? (member (get-cat b) drug-categories) )))
           ((:== #t (a) (pair? (member (get-cat a) gene-or-protein) ))
            (:== #t (b) (pair? (member (get-cat b) gene-or-protein) ))
            (conde ((:== #t (a) (string-prefix? a "HGNC:")))
                   ((:== #t (a) (string-prefix? a "NCBI:")))
                   ((:== #t (a) (string-prefix? a "UniProtKB:")))
                   ((:== #t (a) (string-prefix? a "ENSEMBL:"))))))
    ))

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
            (close-match-synonym a mid)
            (synonym mid b)))
         ((fresh (mid)
            (close-match-synonym b mid)
            (synonym mid a)))         
         ((fresh (mid)
            (direct-synonym a mid)
            (synonym mid b)))
         ((fresh (mid)
            (direct-synonym b mid)
            (synonym mid a)))))

;; Different ways of actually getting synonyms in a reasonable amount of time

(define (synonyms/step term (n 500) (categories '()))
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

(define/memo* (synonyms/set curies)
  (remove-duplicates
   (append curies
           (apply append
                  (map (lambda (curie)
                         (let ((cats (run*/set c (cprop curie "category" c))))
                           (if (not (set-empty? (set-intersect gene-or-protein/set cats)))
                               (set->list (synonyms/breadth curie 1))
                               (run* s (kgx-synonym curie s)))))
                       curies)))))

; define a get-synonyms function based on the synonym relation
; ideally we wanted to start with HGNC prefixes right now because if we start with other curie prefixes
; such as UMLS, we need to increase the steps
(define (get-synonyms a-curie)
  (set->list (run*/set/steps 500 s (synonym a-curie s))))

; get-synonyms-ls function takes a list of curies and return a list of all normalized curies
(define (get-synonyms-ls curie-ls)
  (define (unwrap lst)
    (cond
      [(null? lst) '()]
      [(pair? lst)
       (append (unwrap (car lst)) (unwrap (cdr lst)))]
      [else (list lst)]))
  (time (unwrap (map (lambda (x) (get-synonyms x)) curie-ls))))

; get-names-ls function takes a list of curies and return the curies with their coresponding names
(define (get-names-ls curie-ls)
  (run* (curie name)
    (cprop curie "name" name)
    (membero curie curie-ls)))


(define/memo* (get-synonyms/breadth curie)
  (set->list (synonyms/breadth curie 1)))
(define (synonym-of curie)
  (relation ~synonym-of (s)
    (let* ((cats (run*/set c (cprop curie "category" c)))
           (synonyms (if (not (set-empty? (set-intersect gene-or-protein/set cats)))
                         (get-synonyms/breadth curie)
                         (run* s (kgx-synonym curie s)))))
      (membero s synonyms))))
