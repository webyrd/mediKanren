#lang racket
(require "query.rkt"
         "rank-side-effects.rkt"
         "gene-lists-for-testing.rkt")

(define (unwrap lst)
  (if (null? lst) lst
      (append (car lst) (unwrap (cdr lst)))))
#|
(define gene-curie?
    (lambda (x)
      (or
       (string-prefix? x "HGNC:")
       (string-prefix? x "ENSEMBL:")
       (string-prefix? x "NCBIGene:")
       (string-prefix? x "NCBIGENE:"))))
|#

(define uniprot-curie?
  (lambda (x)
    (string-prefix? x "UniProtKB:")))

;;Converting ENSEMBL curies to UniProtKB curies using synonymization for testing purposes
(define uniprots (remove-duplicates (filter uniprot-curie? (map car (unwrap (set-map ards-genes curie-synonyms/names))))))
(define uniprot-concepts (remf* empty? (find-concepts #t uniprots)))

#|
RUNS 3-hop QUERY IN GO AND RECORDS RESULTS
purpose: to find any regulatory relationships where genes on a given list regulate each other
(results more trusted than 1-hop query)
|#
(define (count-downstream prot-list)
  ;;maps each Uniprot curie (key) in prot-list -> a set of all GO pathways (value) that the UniProt (key) regulates
  (define go-processes (make-hash))
  ;;maps GO pathway curies (key) -> a set of all UniProts (value) that are members of the GO pathway (key) (not intersected with prot-list)
  (define go-process-members (make-hash))
  (define involved_in (keep 1 (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("involved_in")))))
  (define subclass_of (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("subclass_of"))))
  (define positively_regulates (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("positively_regulates"))))
  (define negatively_regulates (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("negatively_regulates"))))
  (for-each
   (lambda (u)
     (define S (filter (lambda (x) (equal? (car x) 'rtx2)) (find-concepts #t (list u))))
     (unless (empty? S)
       (match-define
         (list name=>concepts name=>edges)
         (time (run/graph
                ((G S)
                 (M #f)
                 (X #f)
                 (rG #f))
                ((G->M involved_in)
                 (M->X (set-union positively_regulates negatively_regulates subclass_of))
                 (rG->X involved_in))
                (G G->M M)
                (M M->X X)
                (rG rG->X X))))
       ;;go-processes (k=>v): G=>X
       (for-each
        (lambda (e)
          (define direction 0)
          (cond
            [(set-member? (map cddr positively_regulates) (cdr (edge->pred e))) (set! direction 1)]
            [(set-member? (map cddr negatively_regulates) (cdr (edge->pred e))) (set! direction -1)])
          (define direction-edge-pubmeds '())
          (hash-update! go-processes u (lambda (v) (set-add v (cons (concept->curie (edge->object e))
                                                                    (list direction direction-edge-pubmeds)))) '())
          )
        (hash-ref name=>edges 'M->X)
        )
       ;;go-process-members (k=>v): X=>rG
       (for-each
        (lambda (e)
          (unless (set-member? S (concept->curie (edge->subject e)))
            (define edge-pubmeds '())
            (hash-update! go-process-members (concept->curie (edge->object e))
                          (lambda (v) (set-add v (cons (concept->curie (edge->subject e)) edge-pubmeds))) '())
            )
          )
        (hash-ref name=>edges 'rG->X)
        )
       )
     )
   prot-list
   )
  ;;combine the G=>X and X=>rG hashes into a G=>rG hash with information
  (define G=>rG (make-hash))
  (hash-for-each
   go-processes
   (lambda (k v)
     (for-each
      (lambda (x)
        (for-each
         (lambda (rg)
           (hash-update! G=>rG (assoc k (curie-synonyms/names k))
                         (lambda (val)
                           (set-add val
                                    (list (assoc (car rg) (curie-synonyms/names (car rg)))
                                          (list (cadr x) (caddr x))
                                          (list (assoc (car x) (curie-synonyms/names (car x))) (cdr rg))))) '())
           )
         (hash-ref go-process-members (car x))
         )
        )
      v
      )
     )
   )
  
  (define sorted-G=>rG (sort
                        (hash-map G=>rG (lambda (k v) (cons k v)))
                        >
                        #:key (lambda (x) (length (set-intersect (map caar (cdr x)) prot-list)))))
  sorted-G=>rG
  )
#|Examples
(define ards-test-mini (count-downstream (take uniprots 20)))
(define ards-test-full (count-downstream uniprots))
|#

(define directionless-regulation-preds '("regulates"
                                         "regulates_expression_of"
                                         "regulates_activity_of"
                                         "directly_regulates"
                                         "physically_interacts_with"
                                         "directly_interacts_with"
                                         "biolink:directly_regulates"))

(define pos-regulation-preds '("positively_regulates"
                               "directly_positively_regulates"
                               "biolink:directly_positively_regulates"
                               "biolink:positively_regulates"))

(define neg-regulation-preds '("negatively_regulates"
                               "directly_negatively_regulates"
                               "inhibits"
                               "biolink:directly_negatively_regulates"))

(define direct-interaction-preds '("directly_regulates"
                                   "physically_interacts_with"
                                   "directly_interacts_with"
                                   "directly_positively_regulates"
                                   "directly_negatively_regulates"
                                   "biolink:directly_negatively_regulates"
                                   "biolink:directly_positively_regulates"
                                   "biolink:positively_regulates"
                                   "biolink:directly_regulates"))

(define all-regulation-preds (set-union directionless-regulation-preds
                                        pos-regulation-preds
                                        neg-regulation-preds
                                        direct-interaction-preds))
;;"negatively_regulates__entity_to_entity" and "positively_regulates__entity_to_entity"?

#|
RUNS 1-hop QUERY AND RECORDS RESULTS
purpose: to find any regulatory relationships where genes on a given list regulate each other
|#
(define (find-1-hop-relations genes)
  (define gene-regulation-scores (make-hash))
  (set-for-each genes
                (lambda (g)
                  (define q (time (query/graph
                                   ((G g)
                                    (rG gene-or-protein))
                                   ((G->rG all-regulation-preds))
                                   (G G->rG rG))))
                  (for-each
                   (lambda (e)
                     ;;defining the key as (G rG)
                     (define rg (concept->curie (edge->object e)))
                     ;;record the G->rG interaction direction (up or down regulate)
                     (define direction 0)
                     (cond
                       [(set-member? pos-regulation-preds (cdr (edge->pred e))) (set! direction 1)]
                       [(set-member? neg-regulation-preds (cdr (edge->pred e))) (set! direction -1)])
                     ;;record provenance
                     (define strength-of-direction-edge (pubmed-ids-from-edge e))
                     ;;record whether the predicate indicates direct interaction between the two entities (#t if yes)
                     (define direct-interaction? (set-member? direct-interaction-preds (edge->pred  e)))
                     ;;record provenance
                     (define strength-of-direct-interaction-edge (pubmed-ids-from-edge e))
                     ;;add information to the hash
                     (hash-update! gene-regulation-scores
                                   (assoc g (curie-synonyms/names g))
                                   (lambda (v) (set-add v (list (assoc rg (curie-synonyms/names rg))
                                                                (list direction strength-of-direction-edge)
                                                                (list direct-interaction? strength-of-direct-interaction-edge)
                                                                (concept->curie (edge->subject e))))) '())
                     )
                   (edges/query q 'G->rG)
                   )
                  )
                )
  ;;sort list by the number of regulated genes a gene influences (most to least)
  (define sorted-gene-regulation-scores (sort
                                         (hash-map gene-regulation-scores (lambda (k v) (cons k v)))
                                         >
                                         #:key (lambda (x) (length (cdr x)))))
  
  sorted-gene-regulation-scores
  )
#|Examples
(define 1-hop-mini-test (time (find-1-hop-relations (take ards-genes 10))))
(define 1-hop-full-test (time (find-1-hop-relations ards-genes)))
|#

#|
Finds all cell/tissue types (UBERON) that each gene is expressed in
|#
(define (expression-locations genes-and-proteins)
  (define expression-location (make-hash))
  (for-each
   (lambda (g)
     (define q (time (query/graph
                      ((G-and-rG g)
                       (C #f))
                      ((G-and-rG->C '("expressed_in")))
                      (G-and-rG G-and-rG->C C))))
     (for-each
      (lambda (e)
        (unless (not (string-prefix? (concept->curie (edge->object e)) "UBERON:"))
          (define g (concept->curie (edge->subject e)))
          (define t (concept->curie (edge->object e)))
          (hash-update! expression-location (assoc g (curie-synonyms/names g)) (lambda (v) (set-add v (assoc t (curie-synonyms/names t)))) '())
          )
        )
      (edges/query q 'G-and-rG->C)
      )
     )
   genes-and-proteins
   )
  expression-location
  )
#|Examples
(define cell/tissue-types-mini-test (expression-locations (take uniprots 20)))
(define cell/tissue-types-full-test (expression-locations uniprots))
|#

#|
(define (find-intersections go-query-results 1-hop-query-results)
  (define intersection-points '())
  (for-each
   (lambda (r)
     (filter (lambda (x) (set-member? (curie-synonyms/names (car r)) (car x))) 1-hop-query-results)
     )
   go-query-results
   )
  )
|#

(define pos-drug-preds '("stimulates"))
(define neg-drug-preds '("inhibits"
                         "inhibitor"))
(define drug-preds (set-union pos-drug-preds neg-drug-preds))

#|
(define drug-preds '("inhibitor"
                     "stimulates"
                     ;;^^use these first
                     "targets"
                     "agonist"
                     "antagonist"
                     "indicated_for"
                     "contraindicated_for"
                     "affects"
                     ))
|#

(define (find-all-drug-candidates go-query-results 1-hop-query-results)
  (define drug-targets=>drug (make-hash))
  (for-each
   (lambda (g)
     (define q-drugs (query/graph
                      ((D drug)
                       (G g))
                      ((D->G drug-preds))
                      (D D->G G)))
     (for-each
      (lambda (e)
        (define direction 0) 
        (cond
          [(set-member? pos-drug-preds (cdr (edge->pred e))) (set! direction 1)]
          [(set-member? neg-drug-preds (cdr (edge->pred e))) (set! direction -1)])
        (define drug-curie (concept->curie (edge->subject e)))
        (hash-update! drug-targets=>drug g (lambda (v) (set-add v (list (assoc drug-curie (curie-synonyms/names drug-curie))
                                                                        (list direction (pubmed-ids-from-edge e))))) '())
        )
      (edges/query q-drugs 'D->G)
      )
     )
   (map caar (set-union go-query-results 1-hop-query-results))
   )
  drug-targets=>drug
  )
;;(find-all-drug-candidates '() 1-hop)