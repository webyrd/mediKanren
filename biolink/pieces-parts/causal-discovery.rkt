#lang racket
(require
  "../db.rkt"
  "query.rkt")

;; Note:  we should check if the 'negated' property on an edge is set to 'true'!

;; since synonymization seems to be broken, here is the hand-crafted
;; list of synonyms from RTX2

;; SNOMED --equivalent_to--> MONDO

;; need the UMLS and MONDO from an ICD10 or SNOMED,
;; since UMLS and MONDO have the causal info we need
(define handcrafted-thrombotic-events
  '("MONDO:0005279"
    "MESH:D011655"
    "OMOP:440417"
    "UMLS:C0034065"
    "NCIT:C50713"
    "SNOMED:59282003"
    "EFO:0003827"
    "DOID:9477"))

(define causal-predicates
  '("biolink:causes"
    "biolink:predisposes"
    "causes"
    "contributes_to"
    ))

#|
UMLS --biolink:causes--> UMLS:C0034065   sri_semmeddb

(reverse direction:)
UMLS:C0034065 --biolink:causes--> UMLS   sri_semmeddb

UMLS --biolink:predisposes--> UMLS:C0034065   sri_semmeddb

(reverse direction:)
UMLS:C0034065 --biolink:predisposes--> UMLS   sri_semmeddb

(same preds as above, but without the biolink: prefix)
UMLS --causes--> UMLS:C0034065           semmeddb
UMLS --causes--> UMLS:C0034065           rtx2

CHEBI --contributes_to--> MONDO:0005279   robokop


possibly useful:

(and the revsersed version)
UMLS --biolink:coexists_with--> UMLS:C0034065  sri_semmeddb

(and the revsersed version)
UMLS --biolink:precedes--> UMLS:C0034065   sri_semmeddb

CHEMBL.COMPUND:CHEMBL --indicated_for--> DOID:9477   rtx2
|#

;(define thrombotic-events (curie-synonyms-raw "MONDO:0005279"))

(printf "running query\n")

(define qa (time
            (map
             (lambda (te)
               (query/graph
                ((X #f)
                 (thrombotic-event te))
                ((X->thrombotic-event causal-predicates))
                (X X->thrombotic-event thrombotic-event)))
             handcrafted-thrombotic-events
             )))

#|

(define qb (time (query/graph
                  ((X #f)
                   (thrombotic-event "MONDO:0005279"))
                  ((X->thrombotic-event #f))
                  (X X->thrombotic-event thrombotic-event))))

(printf "found results\n")

; (pretty-print (time (report/query qb)))

(printf "ranking paths\n")

(define r (ranked-paths qb))

(printf "getting counts\n")

(length (curies/query qb 'X))
(length (curies/query qb 'thrombotic-event))
(length (edges/query qb 'X->thrombotic-event))

(map
 (lambda (e)
   (match e
     [`(,dbname ,eid (,scid ,scui ,sname (,scatid . ,scat) ,sprops)
               (,ocid ,ocui ,oname (,ocatid . ,ocat) ,oprops)
               (,pid . ,pred) ,eprops)
      `(,sname ,pred ,oname)]))
 (edges/query qb 'X->thrombotic-event))

(remove-duplicates
 (map
  (lambda (e)
    (match e
      [`(,dbname ,eid (,scid ,scui ,sname (,scatid . ,scat) ,sprops)
                 (,ocid ,ocui ,oname (,ocatid . ,ocat) ,oprops)
                 (,pid . ,pred) ,eprops)
       pred]))
  (edges/query qb 'X->thrombotic-event)))

|#
