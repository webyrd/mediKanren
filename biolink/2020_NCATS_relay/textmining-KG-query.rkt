#lang racket
(provide (all-defined-out))
(require "../pieces-parts/query.rkt"         
         racket/engine)



(define neg-reg (find-predicates (list "biolink:negatively_regulates_entity_to_entity")))

#|test queries |#

;; "biolink:positively_regulates_entity_to_entity"
;; "biolink:negatively_regulates_entity_to_entity"
(define predicate:neg-reg/text-mining
  (find-exact-predicates (list "biolink:negatively_regulates_entity_to_entity")))

(define predicate:neg-reg/text-mining
  (find-exact-predicates (list "biolink:positively_regulates_entity_to_entity")))

(define TC-->X
  (query/graph
   ((X #f)
    (TC "CHEBI:100147"))
   ((TC->X neg-reg))
   (TC TC->X X)))

(define X-->TC
  (query/graph
   ((X #f)
    (TC "CHEBI:100147"))
   ((TC->X neg-reg))
   (TC TC->X X)))


;; "biolink:related_to"

(define predicate:co-occur/text-mining
  (find-exact-predicates (list "biolink:related_to")))

(define X<--co-occurs-->TC
  (query/graph
   ((X #f)
    (TC "MONDO:0007254"))
   ((X->TC predicate:co-occur/text-mining))
   (X X->TC TC)))

(define rqq
  (edges/query qq 'TC->X))
