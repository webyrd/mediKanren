#lang racket

(require
  "create-hashtable-common.rkt")

(provide
  (all-defined-out))


(define uniprotkb-ht (make-hash))
(define uniprotkb-ht-file-path (build-path HASHTABLE_SAVE_DIRECTORY "uniprotkb-hash.rkt"))

(define (fill-uniprotkb-ht!)

  (set! uniprotkb-ht (make-hash))

  (define add-to-ht! (add-concept-key/cid-associations-to-hashtable! uniprotkb-ht))

  (add-to-ht! SEMMED #rx"UNIPROT:([A-Z0-9]+)")
  (add-to-ht! RTX #rx"UniProtKB:([A-Z0-9]+)")
  (add-to-ht! ROBOKOP #rx"UniProtKB:([A-Z0-9]+)")
  ;; apparently orange doesn't have UniProtKB synonyms 

  )

(define (save-uniprotkb-ht!)
  (save-hashtable! uniprotkb-ht uniprotkb-ht-file-path))

(define (load-uniprotkb-ht)
  (load-hashtable uniprotkb-ht-file-path))

(define (load-or-create/save-uniprotkb-ht!)
  (load-or-create/save-hashtable!
    'uniprotkb
    load-uniprotkb-ht
    fill-uniprotkb-ht!
    save-uniprotkb-ht!))



#|
Three supposedly equivalent concepts:

(semmed 244 "UMLS:C0962796" "SLC31A1 protein, human" (2 . "biological_entity") (("umls_type_label" . "['Biologically Active Substance', 'Amino Acid, Peptide, or Protein']") ("xrefs" . "['MESH:C490079', 'NCI:C127860', 'UNIPROT:O15431', 'MTH:NOCODE']") ("id" . "UMLS:C0962796") ("umls_type" . "['T116', 'T123']") ("labels" . "['biological_entity']")))

(rtx 5771 "UniProtKB:O15431" "solute carrier family 31 member 1" (1 . "protein") (("symbol" . "SLC31A1") ("expanded" . "True") ("rtx_name" . "O15431") ("description" . "The protein encoded by this gene is a high-affinity copper transporter found in the cell membrane. The encoded protein functions as a homotrimer to effect the uptake of dietary copper. [provided by RefSeq, Aug 2011].") ("id" . "UniProtKB:O15431") ("accession" . "O15431") ("UUID" . "4be807f8-390c-11e9-8caf-0242ac110004") ("uri" . "http://identifiers.org/uniprot/O15431") ("seed_node_uuid" . "39368ad0-390c-11e9-8caf-0242ac110004")))

(robokop 20027 "HGNC:11016" "SLC31A1" (0 . "(\"named_thing\" \"gene\")") (("locus_group" . "protein-coding gene") ("chromosome" . "9") ("taxon" . "9606") ("gene_family" . "(\"Solute carriers\")") ("location" . "9q32") ("id" . "HGNC:11016") ("gene_family_id" . "(752)") ("equivalent_identifiers" . "(\"HGNC:11016\" \"UniProtKB:A0A024R824\" \"NCBIGENE:1317\" \"UniProtKB:O15431\" \"ENSEMBL:ENSG00000136868\")")))


Another concept:

(rtx 4 "UniProtKB:P36404" "ADP ribosylation factor like GTPase 2" (1 . "protein") (("symbol" . "ARL2") ("expanded" . "True") ("rtx_name" . "P36404") ("description" . "This gene encodes a small GTP-binding protein of the RAS superfamily which functions as an ADP-ribosylation factor (ARF). The encoded protein is one of a functionally distinct group of ARF-like genes. [provided by RefSeq, Jul 2008].") ("id" . "UniProtKB:P36404") ("accession" . "P36404") ("UUID" . "4da5df48-390c-11e9-8caf-0242ac110004") ("uri" . "http://identifiers.org/uniprot/P36404") ("seed_node_uuid" . "39368ad0-390c-11e9-8caf-0242ac110004")))


orange doesn't seem to contain UniProtKB synonyms
|#



#|
> (hash-count uniprotkb-ht)
89196
> (hash-ref uniprotkb-ht "P36404" #f)
'((robokop . 166967) (rtx . 4))
> (hash-ref uniprotkb-ht "11016" #f)
#f
> (hash-ref uniprotkb-ht "A0A024R824" #f)
'((robokop . 20027))
> (hash-ref uniprotkb-ht "O15431" #f)
'((robokop . 20027) (rtx . 5771) (semmed . 244))
|#
