#lang racket
(require "query.rkt"
         "rank-side-effects.rkt"
         "gene-lists-for-rna-seq-test.rkt")

(define (unwrap lst)
  (if (null? lst) lst
      (append (car lst) (unwrap (cdr lst)))))

;;determines equality based on synonymization
(define (synonymous? curie1 curie2)
  (assoc curie1 (curie-synonyms/names curie2))
  )

;;determines if the rG and direction of regulation are equal given two G->rG relationships

(define (genes->uniprots gene-list)
  (define hgncs (remove-duplicates
                 (flatten (map
                           (lambda (e)
                             (time
                              (define hgnc-q (query/graph
                                              ((E e)
                                               (H (lambda (x) (string-prefix? x "HGNC:"))))
                                              ((E->H '("same_as")))
                                              (E E->H H)))
                              (curies/query hgnc-q 'H)
                              )
                             )
                             gene-list
                             ))
                          ))
  ;;hgncs
  (filter (lambda (x) (string-prefix? x "UniProtKB")) (map car (unwrap (map curie-synonyms/names hgncs))))
  )
;;(define uniprots (time (genes->uniprots ards-genes)))

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
  (define G=>rG (make-hash))
  (define involved_in (keep 1 (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("involved_in")))))
  (define subclass_of (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("subclass_of"))))
  (define positively_regulates (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("positively_regulates"))))
  (define negatively_regulates (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("negatively_regulates"))))
  (for-each
   (lambda (u)
     (define S (filter (lambda (x) (equal? (car x) 'rtx2)) (find-concepts #t (list u))))
     ;;(printf "~v\n" S)
     (unless (empty? S)
       (printf "reached\n")
       (match-define
         (list name=>concepts name=>edges)
         (time (run/graph
                ((G #f)
                 (M #f)
                 (X #f)
                 (rG-on-list S))
                ((rG-on-list->X involved_in)
                 (M->X (set-union positively_regulates negatively_regulates subclass_of))
                 (G->M involved_in))
                (rG-on-list rG-on-list->X X)
                (M M->X X)
                (G G->M M))))
       ;;go-processes (k=>v): G=>X
       (for-each
        (lambda (e)
          (define direction 0)
          (cond
            [(set-member? (map cddr positively_regulates) (cdr (edge->pred e))) (set! direction 1)]
            [(set-member? (map cddr negatively_regulates) (cdr (edge->pred e))) (set! direction -1)])
          (define direction-edge-pubmeds (pubmed-ids-from-edge e))
          (hash-update! go-processes u (lambda (v) (set-add v (cons (concept->curie (edge->object e))
                                                                    (list direction direction-edge-pubmeds)))) '())
          )
        (hash-ref name=>edges 'M->X)
        )
       ;;go-process-members (k=>v): X=>rG
       (for-each
        (lambda (e)
          (unless (set-member? S (concept->curie (edge->subject e)))
            (define edge-pubmeds (pubmed-ids-from-edge e))
            (hash-update! go-process-members (concept->curie (edge->object e))
                          (lambda (v) (set-add v (cons (concept->curie (edge->subject e)) edge-pubmeds))) '())
            )
          )
        (hash-ref name=>edges 'rG-on-list->X)
        )
       )
     )
   prot-list
   )
  ;;combine the G=>X and X=>rG hashes into a G=>rG hash with information
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
(define ards-test-mini (time (count-downstream (take uniprots 10))))
(length (remove-duplicates (map car (unwrap (map cdr ards-test-mini)))))
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

(define direct-interaction-preds '("regulates"
                                   "directly_regulates"
                                   "physically_interacts_with"
                                   "directly_interacts_with"
                                   "directly_positively_regulates"
                                   "directly_negatively_regulates"
                                   "biolink:directly_negatively_regulates"
                                   "biolink:directly_positively_regulates"
                                   "biolink:positively_regulates"
                                   "biolink:directly_regulates"
                                   "actively_involved_in"))

(define all-regulation-preds (set-union directionless-regulation-preds
                                        pos-regulation-preds
                                        neg-regulation-preds
                                        direct-interaction-preds))

;;"negatively_regulates__entity_to_entity" and "positively_regulates__entity_to_entity"?

(define (find-1-hop-relations genes)
  (define gene-regulation-scores (make-hash))
  (for-each
   (lambda (g)
     (define q (time (query/graph
                      ((G+ gene-or-protein)
                       (G- gene-or-protein)
                       (G gene-or-protein)
                       (rG-on-list g))
                      ((G+-pos->rG-on-list positively-regulates);;pos-regulation-preds)
                       (G--neg->rG-on-list negatively-regulates)
                       (G->rG-on-list direct-interaction-preds))
                      (G+ G+-pos->rG-on-list rG-on-list)
                      (G- G--neg->rG-on-list rG-on-list)
                      (G G->rG-on-list rG-on-list)
                      )))
     
     ;;(report/query q)
     (for-each
      (lambda (e)
        ;;defining the key as (G rG)
        (define rg (concept->curie (edge->object e)))
        ;;record provenance
        (define direction-provenance (pubmed-ids-from-edge e))
        ;;record whether the predicate indicates direct interaction between the two entities (#t if yes)
        (define direct-interaction? (set-member? direct-interaction-preds (edge->pred  e)))
        ;;record provenance
        (define direct-interaction-provenance '())
        (unless (not direct-interaction?)
          (set! direct-interaction-provenance (pubmed-ids-from-edge e))
          )
        ;;add information to the hash
        (hash-update! gene-regulation-scores
                      (assoc g (curie-synonyms/names g))
                      (lambda (v) (set-add v (list (assoc rg (curie-synonyms/names rg))
                                                   (list 1 direction-provenance)
                                                   (list direct-interaction? direct-interaction-provenance)
                                                   (concept->curie (edge->subject e))))) '())
        )
      (edges/query q 'G+-pos->rG-on-list)
      )
     
     (for-each
      (lambda (e)
        ;;defining the key as (G rG)
        (define rg (concept->curie (edge->object e)))
        ;;record provenance
        (define direction-provenance (pubmed-ids-from-edge e))
        ;;record whether the predicate indicates direct interaction between the two entities (#t if yes)
        (define direct-interaction? (set-member? direct-interaction-preds (edge->pred  e)))
        ;;record provenance
        (define direct-interaction-provenance (pubmed-ids-from-edge e))
        ;;add information to the hash
        (hash-update! gene-regulation-scores
                      (assoc g (curie-synonyms/names g))
                      (lambda (v) (set-add v (list (assoc rg (curie-synonyms/names rg))
                                                   (list -1 direction-provenance)
                                                   (list direct-interaction? direct-interaction-provenance)
                                                   (concept->curie (edge->subject e))))) '())
        )
      (edges/query q 'G--neg->rG-on-list)
      )
     (for-each
      (lambda (e)
        ;;defining the key as (G rG)
        (define rg (concept->curie (edge->object e)))
        ;;record provenance
        (define direction-provenance (pubmed-ids-from-edge e))
        ;;record whether the predicate indicates direct interaction between the two entities (#t if yes)
        (define direct-interaction? (set-member? direct-interaction-preds (edge->pred  e)))
        ;;record provenance
        (define direct-interaction-provenance (pubmed-ids-from-edge e))
        ;;add information to the hash
        (hash-update! gene-regulation-scores
                      (assoc g (curie-synonyms/names g))
                      (lambda (v) (set-add v (list (assoc rg (curie-synonyms/names rg))
                                                   (list 0 direction-provenance)
                                                   (list direct-interaction? direct-interaction-provenance)
                                                   (concept->curie (edge->subject e))))) '())
        )
      (edges/query q 'G->rG-on-list)
      )

     )
   genes
   )
  ;;sort list by the number of regulated genes a gene influences (most to least)
  (define sorted-gene-regulation-scores (sort
                                         (hash-map gene-regulation-scores (lambda (k v) (cons k v)))
                                         >
                                         #:key (lambda (x) (length (set-intersect (map caar (cdr x)) genes)))))
  
  sorted-gene-regulation-scores
  )
#|Examples
(define 1-hop-mini-test (time (find-1-hop-relations (take uniprots 10))))
(define 1-hop-full-test (time (find-1-hop-relations uniprots)))
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

(define (find-intersections go-query-results 1-hop-query-results)
  (define intersection-points (make-hash))
  (for-each
   (lambda (r)
      (define 1-hop-match (assoc (car r) 1-hop-query-results))
     (cond
       [(and 1-hop-match (equal? (cons (cadr 1-hop-match) (caaddr 1-hop-match))
                               (cons (cadr r) (caaddr r))))]
       (hash-update! intersection-points (car r) (lambda (v) (set-add v (cons (cadr r) (caaddr r)))) '()))
     )
   go-query-results
   )
  intersection-points
  )

(define pos-drug-preds '("stimulates"
                         "agonist"
                         "positive_allosteric_modulator"
                         "positive_modulator"
                         "activator"
                         "stimulator"))

(define neg-drug-preds '("inhibits"
                         "inhibitor"
                         "decreases_expression_of"
                         "negative_modulator"
                         "negative_allosteric_modulator"
                         "decreases_activity_of"
                         "antagonist"
                         "blocker"
                         "channel_blocker"
                         ))

(define directionless-drug-preds '("directly_interacts_with"
                                   "physically_interacts_with"
                                   "modulator"
                                   "targets"
                                   "target"))

(define drug-preds (set-union pos-drug-preds neg-drug-preds directionless-drug-preds))

(define (drug->gene-into-hash hash gene-curie edges edge-directionality)
  (for-each
   (lambda (e)
     (define drug-curie (concept->curie (edge->subject e)))
     (hash-update! hash gene-curie (lambda (v) (set-add v (list (assoc drug-curie (curie-synonyms/names drug-curie))
                                                                (list edge-directionality (pubmed-ids-from-edge e))))) '())
     )
   edges
   )
  )


(define (find-all-drug-candidates genes);;go-query-results 1-hop-query-results)
  (define drug-targets=>drug (make-hash))
  (define drug-scores (make-hash))
  (for-each
   (lambda (g)
     (define q-drugs (time (query/graph
                            ((D+ drug)
                             (D- drug)
                             (D drug)
                             (G g)
                             (OT gene-or-protein))
                            ((D+-pos->G pos-drug-preds)
                             (D--neg->G neg-drug-preds)
                             (D->G directionless-drug-preds)
                             (D->OT drug-preds))
                            (D+ D+-pos->G G)
                            (D- D--neg->G G)
                            (D D->G G)
                            (D D->OT OT))))
     (drug->gene-into-hash drug-targets=>drug g (edges/query q-drugs 'D+-pos->G) 1)
     (drug->gene-into-hash drug-targets=>drug g (edges/query q-drugs 'D--neg->G) -1)
     (drug->gene-into-hash drug-targets=>drug g (edges/query q-drugs 'D->G) 0)
     )
   genes ;;(map caar (set-union go-query-results 1-hop-query-results))
   )
  #|
  (for-each
   (lambda (d)
     (define q-off-targets (query/graph
                            ((D d)
                             (all-Gs gene-or-protein))
                            ((D->all-Gs drug-preds))
                            (D D->all-Gs all-Gs)))
     (hash-update! drug-scores (assoc d (curie-synonyms/names d))
                   (lambda (v) (list (curies/query 'affected-G) (curies/query 'SE))) '())
     )
   (map caar (hash-values drug-targets=>drug))
   )
|#
  (list drug-targets=>drug) ;;drug-scores)
  )

;;(define drugs (time (find-all-drug-candidates (take uniprots 10))))