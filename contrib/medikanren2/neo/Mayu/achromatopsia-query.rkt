#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers.rkt"
 racket/match
 racket/set
 racket/pretty
 racket/string)

#|
What treats achromatopsia (color blindness)?

An associated gene:
CNGB3

ACHM3
|#

;; achromatopsia (DOID:13911)
(define all-achromatopsia-curies
  (get-descendent-curies*-in-db
   (curies->synonyms-in-db
    (list "DOID:13911"))))
;; http://www.informatics.jax.org/disease/DOID:13911
;; ICD10CM:H53.51, ICD9CM:368.54, MESH:D003117, NCI:C84528, ORDO:49382, UMLS_CUI:C0152200 
'("DOID:0050679"
  "UMLS:C0339537"
  "MONDO:0014677"
  "MESH:C536021"
  "ORPHANET:16"
  "DOID:13911"
  "MONDO:0010563"
  "ORPHANET:49382"
  "NCIT:C84528"
  "MONDO:0009003"
  "HP:0011516"
  "UMLS:C0152200"
  "MONDO:0012398"
  "DOID:0110009"
  "DOID:0110007"
  "DOID:0110008"
  "DOID:0110010"
  "MONDO:0013465"
  "MONDO:0009875"
  "UMLS:C0302129"
  "MONDO:0018852")

;; achromatopsia 3 (DOID:0110008)
(define all-achromatopsia3-curies
  (get-descendent-curies*-in-db
   (curies->synonyms-in-db
    (list "DOID:0110008"))))
;; http://www.informatics.jax.org/disease/DOID:0110008
;; ACHM1; ACHM3; Pingelapese blindness; RMCH1; rod monochromacy 1; rod monochromatism 1 
'("OMIM:262300"
 "OMIM:MTHU004637"
 "DOID:0110008"
 "MONDO:0009875"
 "OMIM:MTHU005722"
 "OMIM:MTHU028493"
 "OMIM:MTHU002321"
 "OMIM:MTHU023011"
 "OMIM:MTHU023012"
 "OMIM:MTHU023013"
 "OMIM:MTHU023014"
 "OMIM:MTHU036378"
 "UMLS:C1849792"
 "MESH:C536129")
