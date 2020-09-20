#lang racket
(provide (all-defined-out))
(require "../pieces-parts/query.rkt"         
         racket/engine)

#|NOTES FOR SYNONYMIZATION CACHE |#
#|
Safe Predicates to synonymize with
-   rtx2_2020_09_16 (487 . "equivalent_to")
-   rtx2_2020_09_16 (228 . "gene_encodes_gene_product")
-   rtx2_2020_09_16 (737 . "has_gene_product")
-   pr-owl (2 . "biolink:same_as")
-   pr-owl (3 . "biolink:has_gene_template*")
|#

#|test queries|#
;; "biolink:positively_regulates_entity_to_entity"
;; "biolink:negatively_regulates_entity_to_entity"

(define predicate:neg-reg/text-mining
  (list "biolink:negatively_regulates_entity_to_entity"))

(define predicate:pos-reg/text-mining
  (list "biolink:positively_regulates_entity_to_entity"))

(define predicate:co-occur/text-mining
  (list "biolink:related_to"))



(define breast-cancer/MONDO+HPO
  '("HP:0006625"
    "HP:0100013"
    "HP:0100783"
    "MONDO:0006513"
    "MONDO:0006512"
    "MONDO:0004438"
    "MONDO:0000618"
    "MONDO:0000616"
    "MONDO:0000615"
    "MONDO:0000552"
    "MONDO:0002483"
    "MONDO:0002487"
    "MONDO:0002489"
    "MONDO:0002671"
    "MONDO:0002705"
    "MONDO:0002707"
    "MONDO:0002859"
    "MONDO:0002975"
    "MONDO:0003024"
    "MONDO:0003087"
    "MONDO:0003185"
    "MONDO:0003208"
    "MONDO:0003371"
    "MONDO:0003390"
    "MONDO:0003532"
    "MONDO:0003548"
    "MONDO:0003582"
    "MONDO:0003593"
    "MONDO:0003635"
    "MONDO:0003934"
    "MONDO:0003936"
    "MONDO:0003982"
    "MONDO:0003983"
    "MONDO:0004288"
    "MONDO:0004274"
    "MONDO:0004360"
    "MONDO:0004379"
    "MONDO:0004438"
    "MONDO:0021116"
    "MONDO:0021115"
    "MONDO:0021047"
    "MONDO:0021090"
    "MONDO:0016419"
    "MONDO:0006512"
    "MONDO:0006513"
    "MONDO:0006270"
    "MONDO:0003728"
    "MONDO:0006256"
    "MONDO:0006244"
    "MONDO:0006184"
    "MONDO:0006166"
    "MONDO:0006117"
    "MONDO:0006098"
    "MONDO:0004658"
    "MONDO:0004953"
    "MONDO:0004984"
    "MONDO:0004988"
    "MONDO:0005023"
    "MONDO:0005051"
    "MONDO:0005063"
    "MONDO:0005494"
    "MONDO:0005219"
    "MONDO:0005590"
    "MONDO:0005628"
    "MONDO:0006027"
    "MONDO:0006043"
    "MONDO:0006050"
    "MONDO:0006056"
    "MONDO:0006116"))

(define X--related_to-->TC
  (time
   (map (lambda (curie)
          (query/graph
           ((X curie)
            (TC #f))
           ((X->TC #f))
           (X X->TC TC)))
         breast-cancer/MONDO+HPO)))

(map (lambda (edge) (edges/query edge 'X->TC)) X--related_to-->TC)


;; X co-occurs--> breast-cancer MONDO
(edges/query
 (query/graph
  ((X #f)
   (TC "MONDO:0004989"))
  ((TC->X (list "biolink:related_to")))
  (TC TC->X X))
 'TC->X)

#|
co-occurence breast cancer disease concepts

|#

(define X<--co-occurs-->TC/edges
  (edges/query X<--co-occurs-->TC 'X->TC))



#|
(define QUERY:Target-Concept--Predicate-->X
  (lambda
      (concept-ls x-filter p-filter db-filter)
    (define 1-hop/query
      (lambda (concept x-filter p-filter db-filter)
        (printf "\nQUERY/GRAPH RUNNING ON: ~a\n" concept)
        (time         
         (query/graph
          ((X x-filter)
           (TC concept))
          ((TC->X p-filter) (edge/db? db-filter))
          (TC TC->X X)))))
    (cond
      ((null? concept-ls) '())
      (else
       (let* ((1-hop/query-result
               (1-hop/query (car concept-ls) x-filter p-filter db-filter))
              (1hop-edge-ls (edges/query 1-hop/query-result 'TC->X)))         
         (append*
          1hop-edge-ls
          (QUERY:Target-Concept--Predicate-->X (cdr concept-ls) x-filter p-filter db-filter)))))))


#|
minikanren queries for edge types
(time
 (run 1 (eid subject object)
  (fresh (eprops)
    (edgeo
     `(rtx2_2020_09_16 ,eid ,subject ,object (228 . "gene_encodes_gene_product") ,eprops)))))

|#
|#
