#lang racket/base

(provide
  query:Known->Known
  query:Known->X
  query:Known->X-scored
  query:X->Known
  query:X->Known-scored
  query:Known<-X->Known
  query:Known->X->Known
  query:X->Y->Known
  query:Concept
  concept-properties
  concept-property-values
  curie-in-db?
  curies-in-db
  curie->properties
  edge-properties
  edge-property-values
  edge-id->properties
  get-highest-bucket-number)
(require
 "make-query-low-level.rkt"
 racket/match)

(define db-path-under-parent "text-mining-dec-6-2023/nov_6_2023/text_mining.db")

(match-define
  (list
   query:Known->Known
   query:Known->X-scored
   query:Known->X
   query:X->Known-scored
   query:X->Known
   query:Known<-X->Known
   query:Known->X->Known
   query:X->Y->Known
   query:Concept
   concept-properties
   concept-property-values
   curie-in-db?
   curies-in-db
   curie->properties
   edge-properties
   edge-property-values
   edge-id->properties
   get-highest-bucket-number
   )
  (make-query-low-level db-path-under-parent))
