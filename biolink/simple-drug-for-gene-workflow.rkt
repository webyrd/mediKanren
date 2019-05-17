#lang racket

;;; Find drugs that budge a given gene/set of genes up or down

;;; Prototype for more sophisticated automated/semi-automated
;;; workflows and workflow interfaces

(provide
  (all-from-out "mk.rkt")
  (all-from-out "db.rkt")
  (all-from-out "mk-db.rkt")
  (all-from-out "common.rkt")
  (all-defined-out))

(require
  "mk.rkt"
  "db.rkt"
  "mk-db.rkt"
  "common.rkt"
  racket/date
  (except-in racket/match ==)
  (only-in srfi/1 iota))

;;; directions
(define DECREASE 'DECREASE)
(define INCREASE 'INCREASE)
(define DECREASE/INCREASE 'DECREASE/INCREASE)

(define D-G-increases-predicate-names
  '("positively_regulates"
    ;; "causes"
    ;; "produces"  ;;; maybe??
    ;; "causes_condition"
    ;; "causally_related_to"
    ;; "contributes_to"
    ;; "causes_adverse_event"
    ;; "gene_associated_with_condition"
    ;; "gene_mutations_contribute_to"
    ;; "disease_to_gene_association"
    "increases_activity_of"
    "increases_expression_of"
    "increases_molecular_interaction"
    "increases_response_to"
    "increases_secretion_of"
    "increases_stability_of"
    "increases_synthesis_of"
    "increases_transport_of"
    "increases_uptake_of"
    "decreases_degradation_of"
    "posetively_regulates" ;;; robokop typo??
    ;; "positively_regulates__entity_to_entity"
    ;; "increases_molecular_modification_of"
    ;; "increases_localization_of"
    ;; "increases_splicing_of"
    ;; "decreases_mutation_rate_of"
    ;; "predisposes"
    ;; "decreases_metabolic_processing_of"
    ))

(define D-G-decreases-predicate-names
  '("negatively_regulates"
    "prevents"
    "treats"
    "indicated_for"
    "decreases_activity_of"
    "decreases_expression_of"
    "decreases_molecular_interaction"
    "decreases_secretion_of"
    "decreases_synthesis_of"
    "decreases_transport_of"
    "decreases_uptake_of"
    "disrupts"
    "increases_degradation_of"
    ;; "negatively_regulates__entity_to_entity"
    ;; "increases_metabolic_processing_of"
    ;; "decreases_response_to"
    ;; "decreases_localization_of"
    ;; "decreases_molecular_modification_of"
    ))


(define VERSION_STRING "mediKanren Simple Drug-for-Gene Workflow 0.1.0")
(displayln VERSION_STRING)

(displayln "loading HGNC-ID-to-concepts hashtable")
(define hgnc-ip (open-input-file "hgnc-hash.rkt"))
(define hgnc-ht (read hgnc-ip))
(close-input-port hgnc-ip)

(displayln "loading semmed knowledge graph")
(define semmed (make-db "data/semmed"))
(displayln "loading rtx knowledge graph")
(define rtx (make-db "data/rtx"))
(displayln "loading robokop knowledge graph")
(define robokop (make-db "data/robokop"))
(displayln "loading orange knowledge graph")
(define orange (make-db "data/orange"))

(define (simple-drug-for-gene-workflow gene-symbol direction)
  (printf "Running simple-drug-for-target-gene-workflow for gene ~s in direction ~s\n" gene-symbol direction)
  (printf "Trying to find a drug or biological entity that will budge target gene ~s in direction ~s\n" gene-symbol direction)
  (printf "Starting workflow for ~s/~s at date/time: ~a\n" gene-symbol direction (date->string (seconds->date (current-seconds)) #t))

  ;;; Do a concept lookup in Robokop for the gene-symbol as the CUI (exact match, case sensitive).
  ;;; Error is none is found, or if more than one is found.
  ;;; Error if the type of the concept isn't 
  ;;; [Eventually need to be able to recover from error.]
  (printf "Looking up a concept in the Robokop Knowledge graph with the exact concept name ~s...\n" gene-symbol)
  
  ;;; In the case of "KRAS", should produce the concept:
  ;;; 
  ;; (robokop 23561
  ;;          "HGNC:6407"
  ;;          "KRAS"
  ;;          (0 . "(\"named_thing\" \"gene\")")
  ;;          (("locus_group" . "protein-coding gene")
  ;;           ("chromosome" . "12")
  ;;           ("taxon" . "9606")
  ;;           ("location" . "12p12.1")
  ;;           ("gene_family" . "(\"RAS type GTPase family\")")
  ;;           ("id" . "HGNC:6407")
  ;;           ("gene_family_id" . "(389)")
  ;;           ("equivalent_identifiers" . "(\"ENSEMBL:ENSG00000133703\" \"UniProtKB:P01116\" \"NCBIGENE:3845\" \"UniProtKB:I1SRC5\" \"UniProtKB:L7RSL8\" \"HGNC:6407\" \"UniProtKB:G3V5T7\" \"UniProtKB:G3V4K2\" \"UniProtKB:A0A024RAV5\")")))

  ;;; lookup concept in robokop

  ;;; check there is exactly one concept

  ;;; check the type of the concept


  ;;; Next step: extract the HGNC code from the CUI, and extract the ENSEMBL and UniProtKB names (hmm--should we lookup all the UniProtKB equivalents, if there is more than one?  Is that okay?) (What about the NCBIGENE name?  Is that useful at all?)


  
  
  (printf "Ending workflow for ~s/~s at date/time: ~a\n" gene-symbol direction (date->string (seconds->date (current-seconds)) #t))
  )

(simple-drug-for-gene-workflow "KRAS" DECREASE)
