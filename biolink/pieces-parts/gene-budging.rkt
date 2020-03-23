#lang racket
(provide (all-defined-out))
(require "query.rkt")

(define make-directly-regulate-gene
  (lambda (regulation-predicates)
    (lambda (gene-curie)
      (displayln "\nRunning 1-hop up query with concept categories")
      (define q (time (query/graph
                       (
                        ;; concepts
                        (X       drug)
                        (my-gene gene-curie)
			)
                       ;; edges
                       ((X->my-gene regulation-predicates))
                       ;; paths
                       (X X->my-gene my-gene))))
      q)))

(define directly-upregulate-gene (make-directly-regulate-gene positively-regulates))
(define directly-downregulate-gene (make-directly-regulate-gene negatively-regulates))



(define curie-to-anything
  (lambda (curie predicate*)
    ;;(printf "starting curie-to-anything with curie ~s and preds ~s\n" curie predicate*)
    (let ((val (query/graph
                ( ;; concepts
                 (X curie)
                 (T #f))
                ;; edges
                ((X->T predicate*))
                ;; paths      
                (X X->T T))))
      ;;(printf "finished curie-to-anything with curie ~s and preds ~s\n" curie predicate*)
      val)))

(define curie-to-tradenames
  (lambda (curie)
    (curie-to-anything curie '("has_tradename"))))

(define curie-to-clinical-trials
  (lambda (curie)
    (curie-to-anything curie '("clinically_tested_approved_unknown_phase"
                               "clinically_tested_terminated_phase_2"
                               "clinically_tested_terminated_phase_3"
                               "clinically_tested_terminated_phase_2_or_phase_3"
                               "clinically_tested_withdrawn_phase_3"
                               "clinically_tested_withdrawn_phase_2_or_phase_3"
                               "clinically_tested_withdrawn_phase_2"
                               "clinically_tested_suspended_phase_2"
                               "clinically_tested_suspended_phase_3"
                               "clinically_tested_suspended_phase_2_or_phase_3"))))

(define curie-to-indicated_for
  (lambda (curie)
    (curie-to-anything curie '("indicated_for"))))

(define curie-to-contraindicated_for
  (lambda (curie)
    (curie-to-anything curie '("contraindicated_for"))))


(define pubmed-URLs-from-composite-edge
  (lambda (composite-edge)
    ;;(printf "starting pubmed-URLs-from-composite-edge\n")
    (define concrete-edges (list-ref composite-edge 2))
    ;;(printf "concrete-edges length = ~s\n" (length concrete-edges))
    (let ((url-ls (map pubmed-URLs-from-edge concrete-edges)))
      ;;(printf "url-ls = ~s\n" url-ls)
      (remove-duplicates (append* url-ls)))))


(define drug-info-for-curie
  (lambda (curie)
    ;;(printf "starting drug-info-for-curie\n")
    (map
     (lambda (l)
       (match l
         [`(,name . ,q)
          (let ((ls (map curie-synonyms/names (curies/query q 'T))))
            (printf "ls length = ~s\n" (apply + (map length ls)))
            (cons name ls))]))
     (list 
      (cons 'tradenames (curie-to-tradenames curie))
      (cons 'clinical-trials (curie-to-clinical-trials curie))
      (cons 'indicated_for (curie-to-indicated_for curie))
      (cons 'contraindicated_for (curie-to-contraindicated_for curie))))))


(define drug-info-from-composite-edge
  (lambda (composite-edge)    
    (define curie (caar composite-edge))
    ; (printf "curie = ~s\n" curie)
    (define pubmed-URLs (pubmed-URLs-from-composite-edge composite-edge))
    ; (printf "calculating curie-synonyms/names\n")
    (define synonyms/names (curie-synonyms/names curie))
    ; (printf "calculated curie-synonyms/names of length ~s\n" (length synonyms/names))
    (append
     (list (cons 'curie curie))
     (list (cons 'curie-synonyms/names synonyms/names))
     (drug-info-for-curie curie)
     (list (cons 'pubmeds pubmed-URLs)))))

(define drug-info-for-tsv-from-composite-edge
  (lambda (composite-edge)
     (append*
      (map (lambda (edge)
	     (match edge
               [`(,db
                  ,edge-id
                  (,_ ,subject-curie ,subject-name (,_ . ,subject-cat) . ,_)
                  (,_ ,object-curie ,object-name (,_ . ,object-cat) . ,_)
                  (,_ . ,predicate) . ,_)
                (map (lambda (info)
                       (match info
                         [`(,pubmed-url ,pub-date ,subject-score ,object-score ,sentence)
                          (let ((subject-info (map cdr (drug-info-for-curie subject-curie))))
                            (append
                             (list db
                                   subject-curie subject-cat subject-name
                                   predicate
                                   object-name object-cat object-curie
                                   pubmed-url pub-date sentence)
                             subject-info))]))
                     (publications-info-alist-from-edge edge))
                ]))
	   (list-ref composite-edge 2)))))


#|
(pubmed-URLs-from-edge '(semmed 907404 (4417 "UMLS:C0023870" "Lithium" (4 . "chemical_substance") (("umls_type_label" . "(\"Pharmacologic Substance\" \"Element, Ion, or Isotope\")") ("xrefs" . "(\"SNOMEDCT_US:85899009\" \"LNC:LP16175-9\" \"LCH_NW:sh85077577\" \"USPMG:MTHU000811\" \"NCI_NCI-GLOSS:CDR0000476347\" \"MMSL:d00061\" \"MTHSPL:NOCODE\" \"CPM:32027\" \"UNII:9FN79X2M3F\" \"MESH:D008094\" \"SNOMEDCT_US:321719003\" \"PDQ:CDR0000039491\" \"NDFRT:N0000147892\" \"SNM:F-10380\" \"VANDF:4019803\" \"MEDCIN:199593\" \"RCD:XM0lB\" \"SNMI:C-15400\" \"CHV:0000007490\" \"NDDF:001532\" \"RCD:d6...\" \"NDFRT:N0000166224\" \"MTHSPL:9FN79X2M3F\" \"ATC:N05AN01\" \"DRUGBANK:DB01356\" \"LNC:MTHU004197\" \"AOD:0000019454\" \"PSY:28590\" \"CSP:1852-6098\" \"MTH:U002050\" \"LCH:U002722\" \"INCHIKEY:SIAPCJWMELPYOE-UHFFFAOYSA-N\" \"RXNORM:6448\" \"CHEMBL:CHEMBL2146126\" \"NCI:C95186\" \"NCI:C1318\")") ("id" . "UMLS:C0023870") ("umls_type" . "(\"T121\" \"T196\")"))) (7571 "UMLS:C1428785" "KDM1A gene" (2 . "gene") (("umls_type_label" . "(\"Gene or Genome\")") ("xrefs" . "(\"NCI_NCI-HGNC:HGNC:29079\" \"HGNC:HGNC:29079\" \"OMIM:609132\" \"NCI:C78141\" \"MTH:NOCODE\")") ("id" . "UMLS:C1428785") ("umls_type" . "(\"T028\")"))) (10 . "negatively_regulates") (("is_defined_by" . "semmeddb") ("negated" . "False") ("SEMMED_PRED" . "INHIBITS") ("pmids" . "21727907") ("provided_by" . "semmeddb_sulab") ("n_pmids" . "1") ("relation" . "semmeddb:negatively_regulates"))))
|#


(printf "starting upregulation...\n")

(define the-gene-curie "HGNC:29079")



(define (make-dr-query1-up/down direction directly-up/down-regulate-gene)
  (lambda (the-gene-curie the-gene-symbol)

    (printf "*** getting directly ~s for gene CURIE ~s\n" direction the-gene-curie)
    
    (define directly-up/down (time (directly-up/down-regulate-gene the-gene-curie)))
    ;; returns the set of all query results (for X, for gene, for edges X->my-gene, etc.)

    ;; unused
    ;; (define directly-up/down-Xs (curies/query directly-up/down 'X))

    (printf "*** getting edges/X->directly-~s for gene CURIE ~s\n" direction the-gene-curie)
  
    ;; each edge corresponds to an X in Xs
    (define edges/X->directly-up/down (time (edges/ranked (ranked-paths directly-up/down) 0 0)))

    (printf "*** getting directly-~s-drug-info for gene CURIE ~s\n" direction the-gene-curie)
  
    (define directly-up/down-drug-info (time (map drug-info-from-composite-edge edges/X->directly-up/down)))

    (printf "*** getting directly-~s-drug-info-for-tsv for gene CURIE ~s\n" direction the-gene-curie)
  
    (define directly-up/down-drug-info-for-tsv (time (map drug-info-for-tsv-from-composite-edge edges/X->directly-up/down)))

    (printf "*** finished getting directly-~s-drug-info-for-tsv for gene CURIE ~s\n" direction the-gene-curie)
    
    (cons the-gene-curie directly-up/down-drug-info-for-tsv)
        
    ))

(define dr-query1-up (make-dr-query1-up/down 'up directly-upregulate-gene))

(define dr-query1-down (make-dr-query1-up/down 'down directly-downregulate-gene))


(define (dr-query1 the-gene-curie)

  (printf "*** dr-query1 called for gene CURIE ~s\n" the-gene-curie)

  (printf "*** getting gene symbol for gene CURIE ~s\n" the-gene-curie)
  
  (define the-gene-symbol (concept->name (car (find-concepts #t the-gene-curie))))
  
  (printf "*** found gene symbol ~s for gene CURIE ~s\n" the-gene-curie)

  (printf "*** finding up-regulators for gene CURIE ~s\n" the-gene-curie)

  (define up-query-results (time (dr-query1-up the-gene-curie the-gene-symbol)))
    
  (printf "*** finding down-regulators for gene CURIE ~s\n" the-gene-curie)

  (define down-query-results (time (dr-query1-down the-gene-curie the-gene-symbol)))
    
  (printf "*** writing results for gene CURIE ~s\n" the-gene-curie)

  (define my-query-result (append up-query-results down-query-results))
  
  (define (go-tsv)
    (tsv-for my-query-result))
  
  (with-output-to-file (format "~a-budging.tsv" the-gene-symbol)
    go-tsv
    #:exists 'replace)

  (printf "*** finished processing gene CURIE ~s\n" the-gene-curie)
    
  )

(define (dr-query gene-curies)
  (for-each dr-query1 gene-curies))

(define (tsv-for gene-curie/infos-list)
  (printf "~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\n"
  "gene CURIE"
  "db"
  "subject CURIE" "subject category" "subject"
  "predicate"
  "object" "object category" "object CURIE"
  "pub URL" "pub date" "pub sentence"
  "tradenames" "clinical trials" "indicated for" "contraindicated for")
  (for-each (lambda (gene-curie/infos)
	      (let ((the-gene-curie (car gene-curie/infos))
		    (infos (cdr gene-curie/infos)))
		(for-each (lambda (xs)
			    (for-each (lambda (x)
					(apply printf "~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\n" (cons the-gene-curie x)))
				      xs))
			  infos)))
	    gene-curie/infos-list))




; (define the-gene-curies (list "HGNC:11390" "HGNC:13557" "HGNC:2537"))
; (define the-gene-curies (list "HGNC:29079" "HGNC:11390" "HGNC:13557" "HGNC:2537"))

(define the-gene-curies (list "HGNC:29079"))


#|
;; aurkb
(define aurkb-directly-up (directly-upregulate-gene "HGNC:11390"))
;; returns the set of all query results (for X, for gene, for edges X->my-gene, etc.)

(define aurkb-directly-up-Xs (curies/query aurkb-directly-up 'X))

;; each edge corresponds to an X in aurkb-Xs
(define edges/X->aurkb-directly-up (edges/ranked (ranked-paths aurkb-directly-up) 0 0))

(define aurkb-directly-up-drug-info (map drug-info-from-composite-edge edges/X->aurkb-directly-up))
|#

#|
(printf "starting downregulation...\n")

(define kdm1a-directly-down (directly-downregulate-gene "HGNC:29079"))
;; returns the set of all query results (for X, for gene, for edges X->my-gene, etc.)

(define kdm1a-directly-down-Xs (curies/query kdm1a-directly-down 'X))

;; each edge corresponds to an X in kdm1a-Xs
(define edges/X->kdm1a-directly-down (edges/ranked (ranked-paths kdm1a-directly-down) 0 0))

;; 
(define kdm1a-directly-down-drug-info (map drug-info-from-composite-edge edges/X->kdm1a-directly-down))
|#



;;; 2-hop

;; ACE2   HGNC:13557

;; CTSL   HGNC:2537

;; include semmed sentences, when possible  (date would be nice)

