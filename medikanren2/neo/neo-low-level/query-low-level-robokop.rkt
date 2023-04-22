#lang racket/base

(provide
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
(require
 "make-query-low-level.rkt"
 racket/match)
 
(define db-path-under-parent "robokop/full_march_2023/full_Robokop.db")

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
