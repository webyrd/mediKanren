#lang racket

(require
  "create-hashtable-common.rkt")

(provide
  (all-defined-out))

(define ensembl-ht (make-hash))
(define ensembl-ht-file-path (build-path HASHTABLE_SAVE_DIRECTORY "ensembl-hash.rkt"))

(define (fill-ensembl-ht!)

  (set! ensembl-ht (make-hash))

  (define add-to-ht! (add-concept-key/cid-associations-to-hashtable! ensembl-ht))

  ;; apparently semmed doesn't have ENSEMBL synonyms
  ;; apparently rtx doesn't have ENSEMBL synonyms 
  (add-to-ht! ROBOKOP #rx"ENSEMBL:([A-Z0-9]+)")
  (add-to-ht! ORANGE #rx"ENSEMBL:([A-Z0-9]+)")

  )

(define (save-ensembl-ht!)
  (save-hashtable! ensembl-ht ensembl-ht-file-path))

(define (load-ensembl-ht)
  (load-hashtable ensembl-ht-file-path))

(define (load-or-create/save-ensembl-ht!)
  (load-or-create/save-hashtable!
    'ensembl
    load-ensembl-ht
    fill-ensembl-ht!
    save-ensembl-ht!))



#|

apparently semmed doesn't have ENSEMBL synonyms (but semmed does have
OMIM and HGNC, which could be useful for cross-referencing):

(semmed 160183 "UMLS:C1413407" "CHRNB1 gene" (4 . "gene") (("umls_type_label" . "['Gene or Genome']") ("xrefs" . "['MTH:NOCODE', 'OMIM:100710', 'HGNC:HGNC:1961']") ("id" . "UMLS:C1413407") ("umls_type" . "['T028']") ("labels" . "['gene']")))



apparently rtx doesn't have ENSEMBL synonyms



These two are supposedly the same:

(robokop 19861 "HGNC:1961" "CHRNB1" (0 . "(\"named_thing\" \"gene\")") (("locus_group" . "protein-coding gene") ("chromosome" . "17") ("taxon" . "9606") ("gene_family" . "(\"Cholinergic receptors nicotinic subunits\")") ("location" . "17p13.1") ("id" . "HGNC:1961") ("gene_family_id" . "(173)") ("equivalent_identifiers" . "(\"UniProtKB:I3L4N5\" \"UniProtKB:I3L535\" \"ENSEMBL:ENSG00000170175\" \"UniProtKB:P11230\" \"UniProtKB:I3L1T7\" \"UniProtKB:I3L3Q9\" \"NCBIGENE:1140\" \"HGNC:1961\")")))

(orange 13750 "NCBIGene:1140" "CHRNB1" (6 . "(\"gene\")") (("iri" . "http://www.ncbi.nlm.nih.gov/gene/1140") ("synonym" . "(\"acetylcholine receptor, nicotinic, beta 1 (muscle)\" \"Acetylcholine Receptor, Muscle, Beta Subunit\" \"CHOLINERGIC RECEPTOR, NICOTINIC, BETA POLYPEPTIDE 1; CHRNB1\" \"CHRNB1\" \"Chrnb\")") ("in_taxon" . "NCBITaxon:9606") ("same_as" . "(\"ENSEMBL:ENSG00000170175\" \"HGNC:1961\" \"OMIM:100710\" \"Orphanet:119419\")") ("provided_by" . "(\"orphanet.ttl\" \"omim.ttl\")") ("description" . "cholinergic receptor nicotinic beta 1 subunit") ("id" . "NCBIGene:1140")))



Another random Robokop entry:

(robokop 20027 "HGNC:11016" "SLC31A1" (0 . "(\"named_thing\" \"gene\")") (("locus_group" . "protein-coding gene") ("chromosome" . "9") ("taxon" . "9606") ("gene_family" . "(\"Solute carriers\")") ("location" . "9q32") ("id" . "HGNC:11016") ("gene_family_id" . "(752)") ("equivalent_identifiers" . "(\"HGNC:11016\" \"UniProtKB:A0A024R824\" \"NCBIGENE:1317\" \"UniProtKB:O15431\" \"ENSEMBL:ENSG00000136868\")")))
|#
