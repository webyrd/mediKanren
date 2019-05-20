#lang racket

(require
  "create-hashtable-common.rkt")

(provide
  (all-defined-out))

(define omim-ht (make-hash))
(define omim-ht-file-path (build-path HASHTABLE_SAVE_DIRECTORY "omim-hash.rkt"))

(define (fill-omim-ht!)

  (set! omim-ht (make-hash))

  (define add-to-ht! (add-concept-key/cid-associations-to-hashtable! omim-ht))

 (add-to-ht! SEMMED #rx"OMIM:([A-Z0-9]+)") 
  ;; apparently rtx doesn't have OMIM synonyms
  (add-to-ht! ROBOKOP #rx"OMIM:([A-Z0-9]+)")
  (add-to-ht! ORANGE #rx"OMIM:([A-Z0-9]+)")

  )

(define (save-omim-ht!)
  (save-hashtable! omim-ht omim-ht-file-path))

(define (load-omim-ht)
  (load-hashtable omim-ht-file-path))

(define (load-or-create/save-omim-ht!)
  (load-or-create/save-hashtable!
    'omim
    load-omim-ht
    fill-omim-ht!
    save-omim-ht!))



#|

Need to recognize occurrences of the CUI in the "id" field as well as the "same_as"/"xrefs" type fields:

(orange 13749 "OMIM:PS601462" "Myasthenic syndrome, congenital" (2 . "(\"named thing\")") (("iri" . "http://purl.obolibrary.org/obo/OMIM_PS601462") ("provided_by" . "(\"omim.ttl\")") ("id" . "OMIM:PS601462")))

(robokop 172408 "OMIM:607514" "BODY MASS INDEX QUANTITATIVE TRAIT LOCUS 10" (3 . "(\"named_thing\" \"disease\")") (("id" . "OMIM:607514") ("equivalent_identifiers" . "(\"UMLS:C2675659\" \"OMIM:607514\")")))

|#
