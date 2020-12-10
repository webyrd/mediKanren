#lang racket
(require "query.rkt"
         "rank-regulators-gene-lists.rkt"
         racket/hash)
(provide (all-defined-out)
         (all-from-out "query.rkt"
                       "rank-regulators-gene-lists.rkt"
                       racket/hash))

#|
Meaning of symbols/variable names

G: a regulator gene or group of regulator genes. It can be any gene in the human genome that's contained in the
   knowledge graphs. It doesn't have to be on gene list of interest, but it can be.
rG: a regulated gene or group of regulated genes. These are constrained to the gene list of interest, so they must
    be on the list of interest, but may be given in a different curie.
M: a middleman. It is a GO: Biological Process concept. A redundancy in GO requires we go through this node in the
   query to get to X.
X: a GO: Biological Process that rGs are linked to via the "involved_in" predicate.
C: a cell or tissue type. Usually given by a GO: or UBERON: curie.
|#

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

(define involved_in (keep 1 (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("involved_in")))))
(define subclass_of (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("subclass_of"))))
(define positively_regulates (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("positively_regulates"))))
(define negatively_regulates (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("negatively_regulates"))))

(define (uniprot-curie? curie)
  (string-prefix? curie "UniProtKB:"))

(define (gene-curie? curie)
  (string-prefix? curie "HGNC:")
  (string-prefix? curie "ENSEMBL:")
  (string-prefix? curie "NCBIGene:"))

(define (rtx2-concept? concept)
  (equal? 'rtx2 (concept->dbname concept)))

#|
Takes two curies and determines if they are synonymous using curie-synonyms/names.
Correctness depends on the synonymization available in mediKanren.
|#
(define (synonymous-curies? curie1 curie2)
  (assoc curie1 (curie-synonyms/names curie2)))

#|
Checks if a curie is an HGNC or NGBIGene curie. These two gene curies are currently the only two synonymized
with UniProtKB curies (as of 12/4/2020).
|#
(define (synonymized-gene-curie? curie)
  (string-prefix? curie "HGNC:")
  (string-prefix? curie "NCBIGene:"))

#|
Takes a concept and returns a pair with the concept's curie and its name.
Returns: (curie . name)
|#
(define (concept->curie/name concept)
  (cons (concept->curie concept) (concept->name concept)))

#|
Takes a curie and returns itself cons its name.
Returns: (curie . name)
|#
(define (curie->curie/name curie)
  (assoc curie (curie-synonyms/names curie)))

#|
Takes a list and "unwraps" the outer layer of parenthesis of each element in the list. If an element is an atom, it does
nothing to it and retains its relative position in the list. If the input is not a list, unwrap just returns the input.
|#
(define (unwrap lst)
  (cond [(or (null? lst) (not (list? lst))) lst]
        [(list? (car lst)) (append (car lst) (unwrap (cdr lst)))]
        [else (cons (car lst) (unwrap (cdr lst)))]))


(define (hash-edges/curies edges)
  (let ((edge-hash (make-hash)))
    (for-each
     (lambda (e)
       (let ((subj-curie (concept->curie (edge->subject e)))
             (obj-curie (concept->curie (edge->object e))))
         (hash-update! edge-hash subj-curie (lambda (v) (set-add v obj-curie)) '())
         )
       )
     edges
     )
    edge-hash
    )
  )

(define (hash-edges/curies-reverse edges)
  (let ((edge-hash (make-hash)))
    (for-each
     (lambda (e)
       (let ((subj-curie (concept->curie (edge->subject e)))
             (obj-curie (concept->curie (edge->object e))))
         (hash-update! edge-hash obj-curie (lambda (v) (set-add v subj-curie)) '())
         )
       )
     edges
     )
    edge-hash
    )
  )

(define (link-edges/curies edges1 edges2)
  (let ((linked-edges (make-hash))
        (edges1-hash (hash-edges/curies edges1))
        (edges2-hash (hash-edges/curies edges2)))
    (hash-for-each edges1-hash
                   (lambda (k1 v1)
                     (hash-update! linked-edges k1 (lambda (v) (set-union v (hash-ref edges2-hash v1 '()))) '())
                     )
                   )
    linked-edges
    )
  )

(define (link-edges/curies-reverse edges1 edges2)
  (let ((linked-edges (make-hash))
        (edges1-hash (hash-edges/curies-reverse edges1))
        (edges2-hash (hash-edges/curies-reverse edges2)))
    (hash-for-each edges2-hash
                   (lambda (k2 v2)
                     (hash-update! linked-edges k2 (lambda (v) (set-union v (hash-ref edges1-hash v2 '()))) '())
                     )
                   )
    linked-edges
    )
  )

(define (gene-key->uniprot-key gene-key)
  (define direction (cadr gene-key))
  (map (lambda (y) (cons y direction)) (remove-duplicates (filter (lambda (x) (string-prefix? (car x) "UniProtKB"))
                                                                  (curie-synonyms/names (caaar gene-key))))))

#|
Takes a list of gene curies and converts them to their uniprot curies.
Returns: list of uniprot curies
|#
(define (genes->uniprots gene-list)
  (define non-synonymized-gene-curies (filter (lambda (x) (not (synonymized-gene-curie? x))) gene-list))
  (define synonymized-gene-curies (set-subtract gene-list non-synonymized-gene-curies))
  (define non-synonymized-gene-concepts (filter rtx2-concept? (find-concepts #t non-synonymized-gene-curies)))
  (define same-as (find-exact-predicates '("same_as")))
  (define has-gene-product (find-exact-predicates '("has_gene_product")))

  ;;This run/graph converts ENSEMBL:, UMLS:, etc. (curies that don't have edges going to UniProtKB: curies) into
  ;;HGNC and NCBIGene curies.
  (match-define
    (list name=>concepts name=>edges)
    (run/graph
     ((N non-synonymized-gene-concepts)
      (S #f))
     ((N->S same-as))
     (N N->S S)))

  ;;synonymized-gene-concepts is a list of HGNC and NCBIGene concepts, which are the only gene concepts that have "hash_gene_product"
  ;;predicate to UniProtKB concepts (as of 12/8/2020)
  (define synonymized-gene-concepts (set-union (filter rtx2-concept? (find-concepts #t synonymized-gene-curies))
                                               (hash-ref name=>concepts 'S)))

  ;;This run/graph takes HGNC and NCBIGene concepts and finds their UniprotKB products
  (match-define
    (list name=>concepts2 name=>edges2)
    (run/graph
     ((G synonymized-gene-concepts)
      (U #f))
     ((G->U has-gene-product))
     (G G->U U)))
  
  (map concept->curie (hash-ref name=>concepts2 'U))
  )

;;Currrently throws error
(define (hash-uniprots=>genes gene-list)
  (define uniprots=>genes (make-hash))
  
  (define non-synonymized-gene-curies (filter (lambda (x) (not (synonymized-gene-curie? x))) gene-list))
  (define non-synonymized-gene-concepts (filter rtx2-concept? (find-concepts #t non-synonymized-gene-curies)))
  
  (define synonymized-gene-curies (set-subtract gene-list non-synonymized-gene-curies))
  (define synonymized-gene-concepts (filter rtx2-concept? (find-concepts #t synonymized-gene-curies)))
  
  (define same_as (find-exact-predicates '("same_as")))
  (define has_gene_product (find-exact-predicates '("has_gene_product")))

  ;;run/graphs may be able to be combined
  (match-define
    (list name=>concepts name=>edges)
    (run/graph
     ((G non-synonymized-gene-concepts)
      (M #f)
      (U #f))
     ((G->M same_as)
      (M->U has_gene_product))
     (G G->M M)
     (M M->U U)))
    
  (match-define
    (list name=>concepts2 name=>edges2)
    (run/graph
     ((G synonymized-gene-concepts)
      (U #f))
     ((G->U has_gene_product))
     (G G->U U)))

  (hash-union (link-edges/curies-reverse (hash-ref name=>edges 'G->M) (hash-ref name=>edges 'M->U))
              (hash-edges/curies-reverse (hash-ref name=>edges2 'G->U)))
  )

#|
Takes a list of uniprot curies.
These uniprots (once translated into concepts) become the input (rG) to a 3-hop query through GO.
The query has 2 parts, G->M->X and rG->X, but each hop is run as a separate run/graph for efficiency.

Returns a list of 3 hashes that contain all of the edges from each run/graph
|#
(define (query-GO uniprot-list)
  (define U (filter (lambda (x) (equal? (car x) 'rtx2)) (find-concepts #t uniprot-list)))

  ;; rG->X hop
  (match-define
    (list rG/X=>concepts rG/X=>edges)
    (time (run/graph
           ((rG U)
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

#|
Takes the hash tables that contain the edges returned by the 3 run/graphs in the query-GO function.
report-GO-queries takes these edge hashes and returns a sorted association list with the following format:

Returns assoc list (key value): '('(G-curie . G-name) '('('(rG-curie . rG-name) direction) M->X-pubmeds))

In the value, direction is either +1, -1, or 0, and it represents the direction in which G regulates rG, as
indicated by what predicate the edge between them has.
In the value, M->X-pubmeds is a list of the pubmed IDs of the M->X edge (from the second query in the query-GO
function) because that edge is the one that has the directional predicates.
|#
(define (report-GO-queries edge-hashes)
  (match-define (list rG/X=>edges M/X=>edges G/M=>edges) edge-hashes)
  (match-define (list X=>rG M=>X/info G=>rG) (list (make-hash) (make-hash) (make-hash)))

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
                           (lambda (v) (set-add v (list (concept->curie (edge->object e))
                                                        direction
                                                        (pubmed-ids-from-edge e))))
                           '())
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
                 (hash-update! G=>rG
                               (concept->curie/name g)
                               (lambda (v)
                                 (cond
                                   [(assoc (car new-val) v) => (lambda (a)
                                                                 (set-add (set-remove v a)
                                                                          (list (car a)
                                                                                (set-union (cadr a) pubmeds))))]
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
  
  ;;sort list by the number of regulated genes, rG, a gene, G, influences (most to least)
  (sort
   (hash-map G=>rG cons)
   >
   #:key (lambda (x) (length (cdr x))))
  )

#|
This function takes a list of uniprot curies and runs and reports a 1 hop query where the argument is the input to the query.
The 1-hop query is from G->rG, where the value passed into uniprot-list is rG. The G->rG edge is partitioned by 3 groups of
predicates that indicate positive regulation, negative regulation, or direct interaction between the G and rG.

The function returns a sorted association list similar to that returned by report-GO-queries, but with more elements of information.

Returns assoc list (key value): '('(G-curie . G-name) '('('(rG-curie . rG-name) direction) G->rG-pubmeds (direct-interaction? interaction-pubmeds)))

In the value, direction is either +1, -1, or 0, and it represents the direction in which G regulates rG, as
indicated by what predicate the edge between them has.
In the value, G->rG-pubmeds is a list of the pubmed IDs for the edge between particular G and rG concepts.
In the value, direct-interaction? is a boolean that is only #t if there is an edge between the G and rG whose
predicate indicates direct-interaction between the G and rG.
In the value, interaction-pubmeds is a list of the pubmed IDs for the edge that indicates direct interaction
between a G and rG. If direct-interaction? is #f, then interaction-pubmeds is and empty list '().
|#
(define (1-hop-query/report uniprot-list)
  (define 1-hop-hash (make-hash))
  (for-each
   (lambda (rg)
     (define q (time (query/graph
                      ((G+ gene-or-protein)
                       (G- gene-or-protein)
                       (G gene-or-protein)
                       (rG-on-list rg))
                      ((G+-pos->rG-on-list positively-regulates)
                       (G--neg->rG-on-list negatively-regulates)
                       (G->rG-on-list direct-interaction-preds))
                      (G+ G+-pos->rG-on-list rG-on-list)
                      (G- G--neg->rG-on-list rG-on-list)
                      (G G->rG-on-list rG-on-list))))
     
     ;;This function takes a list of edges from the 1-hop-query above and populates the hash table
     (define (1-hop-edges->hash edges direction)
       (for-each
        (lambda (e)
          (define g (concept->curie (edge->subject e)))
          ;;record whether the predicate indicates direct interaction between the two entities (#t if yes)
          (define direct-interaction? (set-member? direct-interaction-preds (edge->pred  e)))
          ;;record provenance
          (define direction-pubmeds (pubmed-ids-from-edge e))
          (define direct-interaction-pubmeds (pubmed-ids-from-edge e))
          ;;add information to the hash
          (hash-update! 1-hop-hash
                        (curie->curie/name g)
                        (lambda (v)
                          (cond
                            [(and (assoc (cons (curie->curie/name rg) direction) v)
                                  (not (empty? direction-pubmeds)))
                             (define rg/info (assoc (cons (curie->curie/name rg) direction) v))
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
  
  ;;sort list by the number of regulated genes, rG, a gene, G, influences (most to least)
  (sort
   (hash-map 1-hop-hash cons)
   >
   #:key (lambda (x) (length (cdr x))))
  )

#|
Takes the results returned by the report-GO-queries and 1-hop-query/report and finds intersection points
between them to coroborate and increase confidence in the information at those intersection points.

An intersection point is qualified as an element from the GO query results and one from the 1-hop query results
that have synonymous Gs and rGs and have the same direction.

Returns an association list of the intersecting G and rG relationships with all of the direction, direct interaction,
and provenance information combined from the GO query results and the 1-hop auery results.

Returns assoc list (key value): '('(G-curie . G-name) '('('(rG-curie . rG-name) direction) direction-pubmeds (direct-interaction? interaction-pubmeds)))
|#
(define (find-intersections go-query-results 1-hop-query-results)
  (define intersections (make-hash))
  (define 1-hop-keys (map car 1-hop-query-results))
  (define 1-hop-uniprot-keys (filter (lambda (x) (uniprot-curie? (car x))) 1-hop-keys))
  (define 1-hop-U=>G-hash (hash-uniprots=>genes (map car (set-subtract 1-hop-keys 1-hop-uniprot-keys))))
  (define intersecting-Gs-uniprot-curies/names (set-intersect (map car go-query-results)
                                                              1-hop-uniprot-keys
                                                              (map curie->curie/name (hash-keys 1-hop-U=>G-hash))))
  (print "~v\n" (length intersecting-Gs-uniprot-curies/names))
  (for-each
   (lambda (u)
     (let ((1-hop-value (cond [(hash-ref 1-hop-U=>G-hash (car u)) => (lambda (gene-curie)
                                                                       (cadr (assoc (curie->curie/name (car gene-curie))
                                                                                    1-hop-query-results)))]
                              [else (cadr (assoc u 1-hop-query-results))]))
           (go-query-value (cadr (assoc u go-query-results))))
       (cond [(equal? (car 1-hop-value) (car go-query-value))
              (let ((value (list (car go-query-value)
                                 (set-union (cadr go-query-value) (cadr 1-hop-value))
                                 (cadr 1-hop-value))))
                    (hash-set! intersections u value '()))
              ])
       )
     (print ".\n")
     )
   intersecting-Gs-uniprot-curies/names
   )
  
  ;;sort list by the number of regulated genes, rG, a gene, G, influences (most to least)
  (sort
   (hash-map intersections cons)
   >
   #:key (lambda (x) (length (cdr x))))
  )

#|
Takes a list of the curies of all of the genes/proteins found by the previous GO queries and 1-hop queries.
Runs a query to find where all of the genes/protiens are expressed.
Returns: a hash table that hashes the (curie .  name) of a cell/tissue type to a list of all of the genes/proteins,
also reported as (curie . name), expressed in that cell/tissue type.
|#
(define (find-expression-locations genes-and-proteins)
  (define expression-location (make-hash))
  (for-each
   (lambda (g)
     (define q (query/graph
                ((G-or-rG g)
                 (C #f))
                ((G-or-rG->C '("expressed_in")))
                (G-or-rG G-or-rG->C C)))
     (for-each
      (lambda (e)
        (define g-or-rg (concept->curie (edge->subject e)))
        (define t (concept->curie (edge->object e)))
        (hash-update! expression-location (curie->curie/name t) (lambda (v) (set-add v (curie->curie/name g-or-rg))) '())
        )
      (edges/query q 'G-or-rG->C)
      )
     )
   genes-and-proteins
   )
  expression-location
  )

#|
Takes the hash returned by find-expression-locations and returns an assoc list sorted most to least by the number
of gene/proteins expressed in a cell/tissue type (a.k.a. the length of the value).
|#
(define (sort-cell/tissue-types-by-G-and-rG cell/tissue-type-hash)
  (sort
   (hash-map cell/tissue-type-hash cons)
   >
   #:key (lambda (x) (length (cdr x))))
  )

#|
Takes the hash returned by find-expression-locations and returns an assoc list sorted most to least by the number
of genes from the original list of interest expressed in a cell/tissue type (a.k.a. the length of the value intersected
with the gene list of interest).
|#
(define (sort-cell/tissue-types-by-rG cell/tissue-type-hash uniprots/names)
  (sort
   (hash-map cell/tissue-type-hash (lambda (k v) (cons k (set-intersect v uniprots/names))))
   >
   #:key (lambda (x) (length (cdr x))))
  )
