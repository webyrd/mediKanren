#lang racket/base

#;(provide
  query:Known->Known
  query:Known->X
  query:X->Known  
  query:Concept
  concept-properties
  concept-property-values
  curie-in-db?
  curies-in-db
  curie->properties
  edge-properties
  edge-property-values
  edge-id->properties
  )
(require
 "make-query-low-level.rkt"
 racket/match)

(define db-path-under-parent "my_kg_from_jsonl.db")

(match-define
  (list
   query:Known->Known
   query:Known->X
   query:X->Known  
   query:Concept
   concept-properties
   concept-property-values
   curie-in-db?
   curies-in-db
   curie->properties
   edge-properties
   edge-property-values
   edge-id->properties
   )
  (make-query-low-level-limited db-path-under-parent))
