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

#;(define db-path-under-parent "rtx-kg2-may-9-2024/rtx-kg2-2.9.0pre/rtx-kg2.db")
(define db-path-under-parent "rtx-kg2-oct-30-2024/rtx-kg2-2.10.1pre/rtx-kg2.db")

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
  (make-query-low-level db-path-under-parent "infores:rtx-kg2"))
