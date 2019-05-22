#lang racket

;;; Find drugs that budge a given gene/set of genes up or down

;;; Prototype for more sophisticated automated/semi-automated
;;; workflows and workflow interfaces

(provide
  (all-from-out "mk.rkt")
  (all-from-out "db.rkt")
  (all-from-out "mk-db.rkt")
  (all-from-out "common.rkt")  
  (all-from-out racket/date)
  (all-defined-out))

(require
  "mk.rkt"
  "db.rkt"
  "mk-db.rkt"
  "common.rkt"
  "create-all-hashtables.rkt"
  racket/date
  (except-in racket/match ==))

;;; Dont' print a quote at the beginning of values.
(print-as-expression #f)

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


(define ROBOKOP_GENE_CATEGORY '(0 . "(\"named_thing\" \"gene\")"))
(define ROBOKOP_EQUIVALENT_IDENTIFIERS_KEY "equivalent_identifiers")


(define VERSION_STRING "mediKanren Marvin Prototype 0.1.0 (Simple Drug-for-Gene Workflow Module)")
(displayln VERSION_STRING)


(printf "loading or creating hashtables, as necessary...\n")
(load-or-create/save-all-hashtables!)


(displayln "loading semmed knowledge graph")
(define semmed (make-db "data/semmed"))
(displayln "loading rtx knowledge graph")
(define rtx (make-db "data/rtx"))
(displayln "loading robokop knowledge graph")
(define robokop (make-db "data/robokop"))
(displayln "loading orange knowledge graph")
(define orange (make-db "data/orange"))

(define (simple-drug-for-gene-workflow gene-symbol-string direction)
  (printf "Running simple-drug-for-target-gene-workflow for gene ~s in direction ~s\n" gene-symbol-string direction)
  (printf "Trying to find a drug or biological entity that will budge target gene ~s in direction ~s\n" gene-symbol-string direction)
  (printf "Starting workflow for ~s/~s at date/time: ~a\n" gene-symbol-string direction (date->string (seconds->date (current-seconds)) #t))

  ;;; Do a concept lookup in Robokop for the gene-symbol-string as the CUI (exact match, case sensitive).
  ;;; Error is none is found, or if more than one is found.
  ;;; Error if the type of the concept isn't 
  ;;; [Eventually need to be able to recover from error.]
  (printf "Looking up a concept in the Robokop Knowledge graph containing the concept name ~s...\n" gene-symbol-string)

  (define robokop-concepts-containing-gene-symbol-name
    (run* (concept)
      (db:~name*-concepto/options
       #t ;;case-sensitive?
       "" ;; chars:ignore
       "" ;; chars:split
       robokop
       (list gene-symbol-string)
       concept)))

  (printf "Found ~s concept(s) in Robokop that contain the concept name ~s:\n\n"
          (length robokop-concepts-containing-gene-symbol-name)
          gene-symbol-string)

  (for-each
    (lambda (c) (pretty-print c) (newline))
    robokop-concepts-containing-gene-symbol-name)
  
  (printf "Filtering out all found concepts whose name doesn't exactly match the string ~s\n" gene-symbol-string)

  (define robokop-concepts-with-exact-gene-name
    (filter
     (lambda (c)
       (match c
         [`(,cid ,cui ,name (,catid . ,cat) ,props)
          (string=? gene-symbol-string name)]))
     robokop-concepts-containing-gene-symbol-name))

  ;; make sure we no longer use the old concept list
  (set! robokop-concepts-containing-gene-symbol-name #f)
 
  (printf "Keeping ~s Robokop concept(s) whose name exactly matches ~s:\n\n"
          (length robokop-concepts-with-exact-gene-name)
          gene-symbol-string)

  (for-each
    (lambda (c) (pretty-print c) (newline))
    robokop-concepts-with-exact-gene-name)

  ;;; In the case of "KRAS", should produce the concept:
  ;;;
  ;; (23561
  ;;  "HGNC:6407"
  ;;  "KRAS"
  ;;  (0 . "(\"named_thing\" \"gene\")")
  ;;  (("locus_group" . "protein-coding gene")
  ;;   ("chromosome" . "12")
  ;;   ("taxon" . "9606")
  ;;   ("location" . "12p12.1")
  ;;   ("gene_family" . "(\"RAS type GTPase family\")")
  ;;   ("id" . "HGNC:6407")
  ;;   ("gene_family_id" . "(389)")
  ;;   ("equivalent_identifiers"
  ;;    .
  ;;    "(\"ENSEMBL:ENSG00000133703\" \"UniProtKB:P01116\" \"NCBIGENE:3845\" \"UniProtKB:I1SRC5\" \"UniProtKB:L7RSL8\" \"HGNC:6407\" \"UniProtKB:G3V5T7\" \"UniProtKB:G3V4K2\" \"UniProtKB:A0A024RAV5\")")))

  
  (if (= (length robokop-concepts-with-exact-gene-name) 1)
      (printf "Exactly 1 Robokop concept has a name that exactly matches ~s, as expected.  Continuing...\n\n" gene-symbol-string)
      (error (format "ERROR  Expected exactly 1 Robokop concept whose name exactly matches ~s.  Found ~s instead."
                     gene-symbol-string
                     (length robokop-concepts-with-exact-gene-name))))
  
  (define robokop-target-gene-concept (car robokop-concepts-with-exact-gene-name))
  ;; make sure we no longer use the old concept list
  (set! robokop-concepts-with-exact-gene-name #f)
    
  (match robokop-target-gene-concept
    [`(,cid ,cui ,name ,category ,props)

     (if (equal? category ROBOKOP_GENE_CATEGORY)
         (printf "The remaining Robokop concept has the expected category, ~s.  Continuing...\n\n" ROBOKOP_GENE_CATEGORY)
         (error (format "ERROR  The remaining Robokop concept has the category ~s rather than the expected category ~s"
                        category
                        ROBOKOP_GENE_CATEGORY))) 
     
     ])

  ;;; Next step: extract the HGNC code from the CUI, and extract the ENSEMBL and UniProtKB names (hmm--should we lookup all the UniProtKB equivalents, if there is more than one?  Is that okay?) (What about the NCBIGENE name?  Is that useful at all?)
  (define robokop-target-gene-concept-CUI/props
    (match robokop-target-gene-concept
      [`(,cid ,cui ,name (,catid . ,cat) ,props)
       (list cui props)]))

  (printf "Extracted CUI and properties from Robokop target gene concept:\n\n~s\n\n" robokop-target-gene-concept-CUI/props)
  
  (define robokop-target-gene-concept-CUI (car robokop-target-gene-concept-CUI/props))

  (printf "Robokop target gene concept CUI: ~s\n" robokop-target-gene-concept-CUI)

  (printf "Checking that Robokop target gene concept CUI is a valid HGNC identifier of the form \"HGNC:<integer>\"\n")
  (if (regexp-match #rx"^HGNC:[0-9]+$" robokop-target-gene-concept-CUI)
      (printf "Robokop target gene concept CUI ~s is a valid HGNC identifier of the form \"HGNC:<integer>\", as expected.  Continuing...\n\n" robokop-target-gene-concept-CUI)
      (error (format "ERROR  Robokop target gene concept CUI ~s is not a valid HGNC identifier of the form \"HGNC:<integer>\""
                     robokop-target-gene-concept-CUI)))

  (define robokop-target-gene-concept-HGNC-ID robokop-target-gene-concept-CUI)

  (define robokop-target-gene-concept-property-list (cadr robokop-target-gene-concept-CUI/props))
   
  (printf "Looking up ~s list in Robokop target gene property list:\n\n~s\n\n" ROBOKOP_EQUIVALENT_IDENTIFIERS_KEY robokop-target-gene-concept-property-list)

  (define robokop-target-gene-concept-equivalent-identifiers
    (let ((v (assoc ROBOKOP_EQUIVALENT_IDENTIFIERS_KEY robokop-target-gene-concept-property-list)))
      (and v (cdr v))))

  (if robokop-target-gene-concept-equivalent-identifiers
      (printf "Found equivalent identifiers list for Robokop target gene, as expected:\n~s\nContinuing...\n\n"
              robokop-target-gene-concept-equivalent-identifiers)
      (error (format "ERROR  Unable to find key ~s in property list ~s"
                     ROBOKOP_EQUIVALENT_IDENTIFIERS_KEY
                     robokop-target-gene-concept-property-list)))

  (printf "Extracting ENSMBL equivalent identifiers...\n") 
  (define robokop-target-gene-concept-ENSEMBL-CUIs
    (regexp-match* #rx"ENSEMBL:[A-Z0-9]+" robokop-target-gene-concept-equivalent-identifiers))
  (printf "Extracted ENSMBL equivalent identifiers:\n~s\n\n" robokop-target-gene-concept-ENSEMBL-CUIs)

  (printf "Extracting HGNC equivalent identifiers...\n") 
  (define robokop-target-gene-concept-HGNC-CUIs
    (regexp-match* #rx"HGNC:[0-9]+" robokop-target-gene-concept-equivalent-identifiers))
  (printf "Extracted HGNC equivalent identifiers:\n~s\n\n" robokop-target-gene-concept-HGNC-CUIs)

  (printf "Extracting OMIM equivalent identifiers...\n") 
  (define robokop-target-gene-concept-OMIM-CUIs
    (regexp-match* #rx"OMIM:[A-Z0-9]+" robokop-target-gene-concept-equivalent-identifiers))
  (printf "Extracted OMIM equivalent identifiers:\n~s\n\n" robokop-target-gene-concept-OMIM-CUIs)
  
  (printf "Extracting UniProtKB equivalent identifiers...\n")
  (define robokop-target-gene-concept-UniProtKB-CUIs
    (regexp-match* #rx"UniProtKB:[A-Z0-9]+" robokop-target-gene-concept-equivalent-identifiers))
  (printf "Extracted UniProtKB equivalent identifiers:\n~s\n\n" robokop-target-gene-concept-UniProtKB-CUIs)


  (define (find-CIDs-for-CUI CUI hashtable CUI-ID-regex)
    (define ID* (regexp-match* CUI-ID-regex CUI #:match-select cadr))
    (define ID #f)
    (if (and (list? ID*) (= (length ID*) 1))
        (set! ID (car ID*))
        (error (format "ERROR  find-CIDs-for-CUI expected ID* to be a singleton list, instead got ~s" ID*)))
    (printf "Looking up ID ~s from CUI ~s in hashtable...\n"
            ID
            CUI)
    (define concept-DB/IDs-for-CUI (hash-ref hashtable ID '()))
    (printf "Found these concept DB/CIDs for ID ~s from CUI ~s in hashtable:\n\n~s\n\n"
            ID
            CUI
            concept-DB/IDs-for-CUI)
    concept-DB/IDs-for-CUI)

  (define (find-CIDs-for-multiple-CUIs CUI* hashtable CUI-ID-regex)
    (let loop ([CUI* CUI*]
               [CID* (set)])
      (cond
        ((null? CUI*)
         (let ((CID* (set->list CID*)))
           (printf "in aggregate, found these CIDs for all CUIs:\n~s\n\n" CID*)))
        (else (let ((CUI (car CUI*))
                    (rest (cdr CUI*)))
                (printf "looking up CIDs for CUI ~s...\n" CUI)
                (let ((res (find-CIDs-for-CUI CUI hashtable CUI-ID-regex)))
                  (printf "found these CIDs for CUI ~s:\n~s\n" CUI res)
                  (loop rest
                        (set-union (list->set res) CID*))))))))
  
  ;; special case: single HGNCI CUI as the Robokop main CUI
  (printf "finding concept IDs for canonical Robolop HGNC CUI...\n")
  (define CIDs-for-single-HGNCI-CUI
    (find-CIDs-for-CUI robokop-target-gene-concept-CUI hgnc-ht #rx"^HGNC:([0-9]+)$"))

  (printf "finding concepts for concept IDs ~s...\n" CIDs-for-single-HGNCI-CUI)
  (define concepts-for-single-HGNCI-CUI
    (apply append
           (map
            (lambda (db-key/cid)
              (let ((db-key (car db-key/cid))
                    (cid (cdr db-key/cid)))
                (let ((db (case db-key
                            ((semmed) semmed)
                            ((rtx) rtx)
                            ((robokop) robokop)
                            ((orange) orange))))
                  (let ((raw-concept-vector (db:cid->concept db cid)))
                    (let ((cui (vector-ref raw-concept-vector 0)))
                      (map
                        (lambda (c) (cons db-key c))
                        (run* (concept)
                          (db:~cui-concepto db cui concept))))))))
            CIDs-for-single-HGNCI-CUI)))
  (printf "found these concepts for concept IDs ~s\n~s\n"
          CIDs-for-single-HGNCI-CUI
          concepts-for-single-HGNCI-CUI)
  
  
  (printf "finding concept IDs for ENSEMBL equivalents...\n")
  (define CIDs-for-ENSEMBL-CUIs
    (find-CIDs-for-multiple-CUIs
      robokop-target-gene-concept-ENSEMBL-CUIs
      ensembl-ht
      #rx"^ENSEMBL:([A-Z0-9]+)$"))

  (printf "finding concept IDs for HGNC equivalents...\n")
  (define CIDs-for-HGNC-CUIs
    (find-CIDs-for-multiple-CUIs
      robokop-target-gene-concept-HGNC-CUIs
      hgnc-ht
      #rx"^HGNC:([0-9]+)$"))

  (printf "finding concept IDs for UniProtKB equivalents...\n")
  (define CIDs-for-UniProtKB-CUIs
    (find-CIDs-for-multiple-CUIs
      robokop-target-gene-concept-UniProtKB-CUIs
      uniprotkb-ht
      #rx"^UniProtKB:([A-Z0-9]+)$"))
  
  
  
  (newline)
  (printf "Ending Marvin workflow for ~s/~s at date/time: ~a\n" gene-symbol-string direction (date->string (seconds->date (current-seconds)) #t))
  (displayln "-----------------------------------------------------")
  (newline)
  (newline)
  )

(simple-drug-for-gene-workflow "KRAS" DECREASE)
