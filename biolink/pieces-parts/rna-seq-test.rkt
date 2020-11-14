#lang racket
(require "query.rkt"
         "rank-side-effects.rkt"
         "gene-lists-for-rna-seq-test.rkt")

(define (unwrap lst)
  (if (null? lst) lst
      (append (car lst) (unwrap (cdr lst)))))

;;determines equality based on synonymization
(define (synonymous-curies? curie1 curie2)
  (assoc curie1 (curie-synonyms/names curie2)))

;;takes a concept and returns (list concept-curie concept-name))
(define (concept->curie/name concept)
  (cons (concept->curie concept) (concept->name concept)))

(define (curie->curie/name curie)
  (assoc curie (curie-synonyms/names curie)))

;;turn gene curie into uniprot curie
(define (gene-key->uniprot-key gene-key)
  (define direction (cadr gene-key))
  (map (lambda (y) (cons y direction)) (remove-duplicates (filter (lambda (x) (string-prefix? (car x) "UniProtKB")) (curie-synonyms/names (caaar gene-key))))))

(define (synonymous-keys? key1 key2)
  ;;check if keys have synonymous curies and same direction
  (unless (or (not (synonymous-curies? (caaar key1) (caaar key2))) (not (equal? (cadr key1) (cadr key2))))
    #t
    )
  )

;;determines if the rG and direction of regulation are equal given two G->rG relationships

(define (ensembl->uniprots gene-list)
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
(define uniprots (time (ensembl->uniprots ards-genes)))

(define ensembl-concepts (find-concepts #t ards-genes))

(define involved_in (keep 1 (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("involved_in")))))
(define subclass_of (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("subclass_of"))))
(define positively_regulates (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("positively_regulates"))))
(define negatively_regulates (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("negatively_regulates"))))

(define (query-GO uniprot-list)
  (define S (filter (lambda (x) (equal? (car x) 'rtx2)) (find-concepts #t uniprots)))

  ;; rG->X hop
  (match-define
    (list rG/X=>concepts rG/X=>edges)
    (time (run/graph
           ((rG S)
            (X #f))
           ((rG->X involved_in))
           (rG rG->X X))))

  ;; M->X hop
  (match-define
    (list M/X=>concepts M/X=>edges)
    (time (run/graph
           ((M #f)
            (X (hash-ref rG/X=>concepts 'X)))
           ((M->X (set-union positively_regulates negatively_regulates subclass_of)))
           (M M->X X))))

  ;; G->M hop
  (define Ms-in-GO (filter (lambda (m) (string-prefix? (concept->curie m) "GO:")) (hash-ref M/X=>concepts 'M)))
  (match-define
    (list G/M=>concepts G/M=>edges)
    (time (run/graph
           ((G #f)
            (M Ms-in-GO))
           ((G->M involved_in))
           (G G->M M))))

  ;; returns a list of the three edge hashes, one from each run/graph
  (list rG/X=>edges M/X=>edges G/M=>edges)
  )
#| Example calls
(define GO-query-edges (time (query-GO uniprots)))
|#

(define (report-GO-queries edge-hashes)
  (match-define (list rG/X=>edges M/X=>edges G/M=>edges) edge-hashes)
  (define X=>rG (make-hash))
  (define M=>X/info (make-hash))
  (define G=>rG (make-hash))

  (time (for-each
         (lambda (e)
           (hash-update! X=>rG (concept->curie (edge->object e))
                         (lambda (v) (set-add v (concept->curie (edge->subject e)))) '())
           )
         (hash-ref rG/X=>edges 'rG->X)
         ))
  
  (time (for-each
         (lambda (e)
           (unless (not (string-prefix? (concept->curie (edge->subject e)) "GO:"))
             (define direction 0)
             (cond
               [(set-member? (map cddr positively_regulates) (cdr (edge->pred e))) (set! direction 1)]
               [(set-member? (map cddr negatively_regulates) (cdr (edge->pred e))) (set! direction -1)])
             (hash-update! M=>X/info (concept->curie (edge->subject e))
                           (lambda (v) (set-add v (list (concept->curie (edge->object e)) direction (pubmed-ids-from-edge e)))) '())
             )
           )
         (hash-ref M/X=>edges 'M->X)
         ))

  (time (for-each
         (lambda (g->m)
           (for-each
            (lambda (x/info)
              (match-define (list x direction pubmeds) x/info)
              (for-each
               (lambda (rg)
                 (define g (edge->subject g->m))
                 (define new-val (list (cons (curie->curie/name rg) direction) pubmeds))
                 (hash-update! G=>rG (concept->curie/name g)
                               (lambda (v)
                                 (cond
                                   [(and (not (empty? pubmeds))
                                         (assoc (car new-val) v)) => (lambda (x)
                                                                       (set-add (set-remove v x)
                                                                                (list (car x) (set-union (cadr x) pubmeds))))]
                                   [else (set-add v new-val)])) '())
                 )
               (hash-ref X=>rG x)
               )
              )
            (hash-ref M=>X/info (concept->curie (edge->object g->m)))
            )
           )
         (hash-ref G/M=>edges 'G->M)
         ))

  (sort
   (hash-map G=>rG (lambda (k v) (cons k v)))
   >
   #:key (lambda (x) (length (cdr x))))
  )
#|
(define go-query-results (time (report-GO-queries GO-query-edges)))
|#

#|
(define G=>rG (sort
               (hash-map results-all-2 (lambda (k v) (cons k v)))
               >
               #:key (lambda (x) (length (cdr x)))))

;; shows rG's with different directions of regulation together in the value
(define G=>rG-grouped (map (lambda (g=>rg) (cons (car g=>rg) (unwrap (group-by (lambda (rg/info) (caar rg/info)) (cdr g=>rg))))) G=>rG))

;; shows G's ranked with counts of number of relationships with rG's
(define ranked-Gs-indistinct (map (lambda (x) (cons (car x) (length (cdr x)))) G=>rG))

;; shows G's ranked with counts of number of relationships with distinct rG's
(define ranked-Gs-distinct (map (lambda (g=>rg) (cons (car g=>rg) (length (group-by (lambda (rg/info) (caar rg/info)) (cdr g=>rg))))) go-query-results))

;; shows the genes that are in top 10 of ranked G's that are also on ARDS gene list
(define top-10-Gs-on-list (map curie->curie/name (set-intersect (map caar (take ranked-Gs 10)) uniprots)))

;;460 G's on list of interest
(define Gs-on-list (set-intersect (map caar ranked-Gs) uniprots))
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

#|
1-hop query
|#
(define (1-hop-query/report uniprot-list)
  (define gene-regulation-scores (make-hash))
  (for-each
   (lambda (rg)
     (define q (time (query/graph
                      ((G+ gene-or-protein)
                       (G- gene-or-protein)
                       (G gene-or-protein)
                       (rG-on-list rg))
                      ((G+-pos->rG-on-list positively-regulates);;pos-regulation-preds)
                       (G--neg->rG-on-list negatively-regulates)
                       (G->rG-on-list direct-interaction-preds))
                      (G+ G+-pos->rG-on-list rG-on-list)
                      (G- G--neg->rG-on-list rG-on-list)
                      (G G->rG-on-list rG-on-list))))

     (define (1-hop-edges->hash edges direction)
       (for-each
        (lambda (e)
          ;;defining the g
          (define g (concept->curie (edge->subject e)))
          ;;defining the rg
          ;;(define rg (concept->curie (edge->object e)))
          ;;record provenance
          (define direction-pubmeds (pubmed-ids-from-edge e))
          ;;record whether the predicate indicates direct interaction between the two entities (#t if yes)
          (define direct-interaction? (set-member? direct-interaction-preds (edge->pred  e)))
          ;;record provenance
          (define direct-interaction-pubmeds (pubmed-ids-from-edge e))
          ;;add information to the hash
          ;;(printf "reached\n")
          (hash-update! gene-regulation-scores
                        (curie->curie/name g)
                        (lambda (v)
                          (cond
                            [(and (assoc (cons (curie->curie/name rg) direction) v)
                                  (not (empty? direction-pubmeds))) (define rg/info (assoc (cons (curie->curie/name rg) direction) v))
                                                                    (set-add (set-remove v rg/info)
                                                                             (list (car rg/info)
                                                                                   (set-union (cadr rg/info) direction-pubmeds)
                                                                                   (cons direct-interaction? direct-interaction-pubmeds)))]
                            [else (set-add v (list (cons (curie->curie/name rg) direction)
                                                   direction-pubmeds
                                                   (cons direct-interaction? direct-interaction-pubmeds)))])
                          ) '())
          )
        edges
        )
       )
     
     (1-hop-edges->hash (edges/query q 'G+-pos->rG-on-list) 1)
     (1-hop-edges->hash (edges/query q 'G--neg->rG-on-list) -1)
     (1-hop-edges->hash (edges/query q 'G->rG-on-list) 0)
     )
   uniprot-list
   )
  ;;sort list by the number of regulated genes a gene influences (most to least)
  (sort
   (hash-map gene-regulation-scores (lambda (k v) (cons k v)))
   >
   #:key (lambda (x) (length (cdr x))))
  )
#|
(define 1-hop-mini-test (time (1-hop-query/report (take uniprots 100))))
(define 1-hop-results (time (1-hop-query/report uniprots)))
|#

#|
(define (find-intersections go-query-results 1-hop-query-results)
  (define intersection-points (make-hash))
  (for-each
   (lambda (1-hop-g=>rg)
     (define qo-query-match (assoc (car 1-hop-g=>rg) go-query-results))
     (define go-query-rgs (cdr qo-query-match))
     (define 1-hop-rgs (cdr 1-hop-g=>rg))
     
     
     (cond
       [(and 1-hop-match (equal? (cons (cadr 1-hop-match) (caaddr 1-hop-match))
                                 (cons (cadr r) (caaddr r))))]
       (hash-update! intersection-points (car r) (lambda (v) (set-add v (cons (cadr r) (caaddr r)))) '()))
     )
   1-hop-query-results
   )
  intersection-points
  )
|#