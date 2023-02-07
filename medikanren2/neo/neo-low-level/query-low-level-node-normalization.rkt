#lang racket/base

(provide
  query:Known->X
  query:X->Known  
  curie-in-db?
  )
(require
 "make-query-low-level.rkt"
 racket/match)

(define db-path-under-parent "kgx-synonym/neo/node_normalization.db")

(match-define
  (list
    query:Known->Known
    query:Known->X
    query:X->Known
    query:Known<-X->Known
    query:Known->X->Known
    query:X->Y->Known
    query:Concept
    concept-properties
    concept-property-values
    curie-in-db?
    curie->properties
    edge-properties
    edge-property-values
    edge-id->properties)
  (make-query-low-level db-path-under-parent))