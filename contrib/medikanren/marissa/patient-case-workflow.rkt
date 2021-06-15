#lang racket
(require "../../medikanren/pieces-parts/query.rkt")

(define (unwrap lst)
  (cond [(or (null? lst) (not (list? lst))) lst]
        [(list? (car lst)) (append (car lst) (unwrap (cdr lst)))]
        [else (cons (car lst) (unwrap (cdr lst)))]))

(define (PR-prefix? curie)
  (string-prefix? curie "PR:"))

(define (curie->curie/name curie)
  (assoc curie (curie-synonyms/names curie)))

(define (concept->curie/name concept)
  (cons (concept->curie concept) (concept->name concept)))

(define (edges->preds edges)
  (remove-duplicates (map edge->pred edges)))
  

(define (summarize-curie curie)
  (let ([concepts (find-concepts #t (list curie))])
    (map (lambda (c) (list (concept->dbname c) (concept->curie c) (concept->name c) (concept->category c))) concepts)))

(define cell-categories '(;;rtx2
                          "biolink:CellularComponent"
                          "biolink:Cell"
                          "biolink:OntologyClass|biolink:CellularComponent"
                          "biolink:OntologyClass|biolink:Cell"
                          ;;robokop
                          "\"cellular_component\""
                          "\"cell\""
                          ;;semmed
                          "cell"
                          "cell_component"))

(define subclass-of '("biolink:subClassOf"
                      "subclass_of"))

(define subclass-of-r/g (find-predicates subclass-of))

(define pos/neg-regulates-entity-to-entity '("biolink:negatively_regulates_entity_to_entity"
                                             "biolink:positively_regulates_entity_to_entity"))

;;----------------------------------------Traversing PR----------------------------------------------
#|HGNC -> PR -> general PR code
  Drug or gene/protein -> general PR code|#

(define hum-P450 "PR:P10635")
(define hum-TP53 "PR:P04637")

#|
Given a PR curie, uses query/graph to traverse the textminingprovider kg to find the parent curie or
super curie.

Returns a list of the super curie, name pairs.
|#
(define (find-supers-q/g curie)
  (let ([q (query/graph
            ((Sub curie)
             (Sup #f))
            ((Sub->Sup subclass-of))
            (Sub Sub->Sup Sup))])
    (map curie->curie/name (curies/query q 'Sup))))

#|
Does the same thing as (find-supers-q/g) except using run/graph.
|#
(define (find-supers-r/g curie)
  (define concept (find-concepts #t (list curie)))
  (match-define
    (list concepts edges)
    (run/graph
     ((Sub concept)
      (Sup #f))
     ((Sub->Sup subclass-of-r/g))
     (Sub Sub->Sup Sup)))
  
  (map concept->curie/name (hash-ref concepts 'Sup)))

#|
Takes a PR curie and uses query/graph to find its subclasses.

Returns a list of subclass curie, name pairs.
|#
(define (find-subclasses curie)
  (let ([q (query/graph
            ((Sub #f)
             (Sup curie))
            ((Sub->Sup subclass-of))
            (Sub Sub->Sup Sup))])
    (map curie->curie/name (curies/query q 'Sub))))

#|
Takes a list of human genes and uses query/graph to find their orthologs.

Returns a list of ortholog curie, name pairs.
|#
(define (find-orthologs superclass-curie)
  (let ([q (query/graph
            ((O #f)
             (Sup superclass-curie))
            ((O->Sup subclass-of))
            (O O->Sup Sup))])
    (map curie->curie/name (curies/query q 'O))))

#|
Takes a PR curie and uses query/graph to find any drugs that positively or negatively regulate
that gene or ortholog.
|#
(define (find-drugs/chems curie)
  (let ([q (query/graph
            ((D/G #f)
             (P/G curie))
            ((D/G->P/G pos/neg-regulates-entity-to-entity))
            (D/G D/G->P/G P/G))])
    (map curie->curie/name (curies/query q 'D/G))))

#|Examples-----------------------------------------------------------------------------
(find-supers-q/g hum-TP53)
;; User selects correct curie from dropdown, something along those lines
(define super-TP53 "PR:000003035")
(find-orthologs super-TP53)
;; User selects all orthologs they're interested in
(map (lambda (x) (find-drugs/chems (car x))) (find-orthologs super-TP53))
(find-drugs/chems super-TP53)

(find-supers-q/g hum-P450)
;; User selects correct curie from dropdown, something along those lines
(define super-P450 "PR:000006121")
(find-orthologs super-P450)
|#