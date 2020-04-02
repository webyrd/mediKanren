#lang racket
(provide (all-defined-out))
(require "query.rkt"
         racket/engine)

;;get simple pieces of edges
(define edge-matcher
  (lambda (ls els)
    (cond
      ((null? ls) (set-union els))
      (else
       (match (car ls)
         [`(,db ,edge-cui
               (,subject-cui ,subject-id ,subject-name (,_ . ,subject-category) ,subject-props-assoc)
               (,object-cui ,object-id ,object-name (,_ . ,object-category) ,object-props-assoc)
               (,_ . ,pred)
               ,pred-props-assoc)
          (edge-matcher
           (cdr ls)
           (set-union
            (list `((,db) (,subject-name . ,subject-id) ,pred (,object-name . ,object-id))) 
            els))])))))

(define gene-filter
  (lambda (ls els)
    (cond
      ((null? ls) (set-union els))
      ((or (string-prefix? (car ls) "HGNC:")
           (string-prefix? (car ls) "ENSEMBL:")
           (string-prefix? (car ls) "UniProtKB:")
           (string-prefix? (car ls) "NCBIGene:")
           (string-prefix? (car ls) "NCBIGENE:"))
       (gene-filter
        (cdr ls)
        (cons (car ls) els)))
      (else
       (gene-filter (cdr ls) els)))))

(define drug-filter
  (lambda (ls els)
    (cond
      ((null? ls) (set-union els))
      ((or (string-prefix? (car ls) "CHEBI:")
           (string-prefix? (car ls) "CHEMBL:")
           (string-prefix? (car ls) "CHEMBL.")
           (string-prefix? (car ls) "KEGG:")
           (string-prefix? (car ls) "KEGG.")
           (string-prefix? (car ls) "DRUGBANK:")
           (string-prefix? (car ls) "RXNORM:"))
       (drug-filter
        (cdr ls)
        (cons (car ls) els)))
      (else
       (drug-filter (cdr ls) els)))))

(define filter/curie
  (lambda (ls els curie)
    (cond
      ((null? ls) (set-union els))
      ((string-prefix? (car ls) curie)
       (filter/curie
        (cdr ls)
        (cons (car ls) els) curie))
      (else
       (filter/curie (cdr ls) els curie)))))

(define remove-item
  (lambda (x ls els)
    (cond
      ((null? ls) (reverse els))
      ((or (boolean? (car ls))
           (void? (car ls)))
       (remove-item
        x (cdr ls) els))
      ((equal? x (car ls))
       (remove-item x (cdr ls) els))
      (else
       (remove-item x (cdr ls)
                    (cons (car ls) els))))))

#|
,en synonymize.rkt
,en query.rkt
|#

;; 670 edges
(define ACE2 (time (query/graph
                  ((X       #f)
                   (ACE2 "HGNC:13557"))
                  ((X->ACE2 #f))
                  (X X->ACE2 ACE2))))

;; all X's in the X--pred-->ACE2 edges
(define 1-hop/concepts->ACE2 (curies/query ACE2 'X))

;; gives full edge list X--pred-->ACE2
(define X->ACE2 (edges/query ACE2 'X->ACE2))

;; gives db S P O of all edges 
(define X->ACE2/simple
  (edge-matcher X->ACE2 '()))

;;all unique predicates in the X->ACE2 edges, only 29 unique ones
(define X->ACE2/preds
  (remove-duplicates (map (lambda (ls) (list-ref ls 2)) X->ACE2/simple)))

;; all Gene concept X's + synonyms in the X--pred-->ACE2 edges
;; seems like there are 57 gene concepts
(define 1-hop-affector-genes/ACE2
  (remove-item
   '()
   (map (lambda (ls) (gene-filter ls '()))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->ACE2))) '()))

;; seems like there is a 1 to 1 ratio with HGNC ids, there are 57 HGNCs
(define 1-hop-affector-genes-HGNC/ACE2
  (remove-item
   '()
   (map (lambda (ls) (filter/curie ls '() "HGNC:"))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->ACE2))) '()))


;; 108 total: all Drug concept X's + synonyms in the X--pred-->ACE2 edges
;; question: what curie can we use to isolate drugs we want? DRUGBANK? CHEBI?
(define 1-hop-affector-drugs/ACE2
  (remove-item
   '()
   (map (lambda (ls) (drug-filter ls '()))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->ACE2))) '()))

(define 1-hop-affector-drugs-CHEBI/ACE2 
  (remove-item
   '()
   (map (lambda (ls) (filter/curie ls '() "CHEBI:"))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->ACE2))) '()))

;; 37 DRUGBANK curies  
(define 1-hop-affector-drugs-DRUGBANK/ACE2 
  (remove-item
   '()
   (map (lambda (ls) (filter/curie ls '() "DRUGBANK:"))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->ACE2))) '()))




#|start 2-hop affector gene code here|#
#|
1-hop-affector-genes-HGNC/ACE2

(define f
  (lambda (target-gene-ls number-of-hops els)
    (cond
      ((= number-of-hops 0) els)
      (define 1-hop/query
        (query/graph
         ((X #f))
         ((X->TG #f)
          (X X->TG (car target-gene-ls)))))
      )))
|#



#|
;; 390 edges
(define TMPRSS2 (time (query/graph
                  ((X       #f)
                   (TMPRSS2 "HGNC:11876"))
                  ((X->TMPRSS2 #f))
                  (X X->TMPRSS2 TMPRSS2))))

(define 1-hop/concepts->TMPRSS2 (curies/query TMPRSS2 'X))

;; gives full edges
(define X->TMPRSS2 (edges/query TMPRSS2 'X->TMPRSS2))

(define X->TMPRSS2/simple
  (edge-matcher X->TMPRSS2 '()))

(define X->TMPRSS2/preds
  (remove-duplicates (map (lambda (ls) (list-ref ls 2)) X->TMPRSS2/simple)))

(define 1-hop-affector-genes/TMPRSS2
  (remove-item
   '()
   (map (lambda (ls) (gene-filter ls '()))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->TMPRSS2))) '()))

(define 1-hop-affector-drugs/TMPRSS2
  (remove-item
   '()
   (map (lambda (ls) (drug-filter ls '()))
        (map set->list (map (lambda (ls) (curie-synonyms ls)) 1-hop/concepts->TMPRSS2))) '()))


;; 2236 edges 
(define CXCL10 (time (query/graph
                  ((X       #f)
                   (CXCL10 "HGNC:10637"))
                  ((X->CXCL10 #f))
                  (X X->CXCL10 CXCL10))))

(define 1-hop/concepts->CXCL10 (curies/query CXCL10 'X))

;; gives full edges
(define X->CXCL10 (edges/query CXCL10 'X->CXCL10))

(define X->CXCL10/simple
  (edge-matcher X->CXCL10 '()))

(define X->CXCL10/preds
  (remove-duplicates (map (lambda (ls) (list-ref ls 2)) X->CXCL10/simple)))

|#


#|
;;manually filtered list for X--decreases-->ACE2
'("targets"
  "inhibitor"
  "physically_interacts_with"
  "regulates_expression_of"
  "interacts_with"
  "directly_interacts_with"
  "decreases_activity_of"
  "affects"
  "associated_with"
  "inhibits"
  "coexists_with"
  "compared_with"
  "negatively_regulates")

;;manually filtered list for X--increases-->ACE2
'("targets"
  "physically_interacts_with"
  "regulates_expression_of"
  "interacts_with"
  "directly_interacts_with"
  "activator"
  "affects"
  "associated_with"
  "produces"
  "stimulates"
  "coexists_with"
  "positively_regulates"
  "positively_regulates__entity_to_entity")

|#



#|
#|GENE BUDGER CODE STARTS HERE|#
;; ACE2 = curie for gene-budger code
;; how is the budger code built? Do you take a query built q -- an edge and 


;;creates a query/graph shell without the curie, gene-curie has yet to be defined
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


;; creates the upregulates and downregulates query/graph shells
(define directly-upregulate-gene (make-directly-regulate-gene positively-regulates))
(define directly-downregulate-gene (make-directly-regulate-gene negatively-regulates))

;;creates the shell to plug in a predicate once we have synonymized curies 
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


;; gives trade name for MTHSPL drug
(define curie-to-tradenames
  (lambda (curie)
    (curie-to-anything curie '("has_tradename"))))

;; gives clinical trial info for DRUGBANK concepts 
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

;; for MESH
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
    (printf "*** starting drug-info-for-curie ~s\n" curie)
    (map
     (lambda (l)
       (match l
         [`(,name . ,q)
          (printf "*** calculating curie-synonyms/names list for curie ~s\n" curie)
          (let ((ls (time (map curie-synonyms/names (curies/query q 'T)))))
            (printf "*** calculated curie-synonyms/names list for curie ~s\n" curie)
            (printf "*** ls length = ~s\n" (apply + (map length ls)))
            (cons name ls))]))
     (list 
      (cons 'tradenames (curie-to-tradenames curie))
      (cons 'clinical-trials (curie-to-clinical-trials curie))
      (cons 'indicated_for (curie-to-indicated_for curie))
      (cons 'contraindicated_for (curie-to-contraindicated_for curie))))))


(define drug-info-for-tsv-from-composite-edge
  (lambda (composite-edge)
    (match composite-edge
      (`((,composite-subject . ,composite-object) ,score ,edges)
        (define subject-info (map cdr (drug-info-for-curie composite-subject)))
        (append*
          (map (lambda (edge)
                 (define pub-info (publications-info-alist-from-edge edge))
                 (define pub-urls (pubmed-URLs-from-edge edge))
                 (match edge
                   [`(,db
                       ,edge-id
                       (,_ ,subject-curie ,subject-name (,_ . ,subject-cat) . ,_)
                       (,_ ,object-curie ,object-name (,_ . ,object-cat) . ,_)
                       (,_ . ,predicate) . ,_)
                     (define (entry pubmed-url pub-date sentence)
                       (append
                         (list db
                               subject-curie subject-cat subject-name
                               predicate
                               object-name object-cat object-curie
                               pubmed-url pub-date sentence)
                         subject-info))
                     (cond ((pair? pub-info)
                            (map (lambda (info)
                                   (match info
                                     [`(,pubmed-url ,pub-date ,subject-score ,object-score ,sentence)
                                       (entry pubmed-url pub-date sentence)]))
                                 pub-info))
                           ((pair? pub-urls)
                            (map (lambda (url) (entry url "" "")) pub-urls))
                           (else (list (entry "" "" ""))))]))
               edges))))))


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

    directly-up/down-drug-info-for-tsv
        
    ))

(define dr-query1-up (make-dr-query1-up/down 'up ACE2))

(define dr-query1-down (make-dr-query1-up/down 'down ACE2))


(define (dr-query1 the-gene-curie)

  (printf "*** dr-query1 called for gene CURIE ~s\n" the-gene-curie)

  (printf "*** getting gene symbol for gene CURIE ~s\n" the-gene-curie)
  
  (define the-gene-symbol (concept->name (car (find-concepts #t (list the-gene-curie)))))
  
  (printf "*** found gene symbol ~s for gene CURIE ~s\n" the-gene-symbol the-gene-curie)

  (printf "*** finding up-regulators for gene CURIE ~s\n" the-gene-curie)

  (define up-query-results (time (dr-query1-up the-gene-curie the-gene-symbol)))
    
  (printf "*** finding down-regulators for gene CURIE ~s\n" the-gene-curie)

  (define down-query-results (time (dr-query1-down the-gene-curie the-gene-symbol)))
  
  (define my-query-result (append up-query-results down-query-results))
  
  (define output-file-name (format "~a-budging.tsv" the-gene-symbol))
  
  (printf "*** writing results for gene CURIE ~s to file ~s\n" the-gene-curie output-file-name)
  
  (with-output-to-file output-file-name
    (tsv-for the-gene-curie the-gene-symbol my-query-result)
    #:exists 'replace)

  (printf "*** finished processing gene CURIE ~s\n" the-gene-curie)

  'finished
  )

(define (dr-query gene-curies)
  (for-each
    (lambda (curie)
      ;; 10 minute timeout per curie
      (define timeout-ms (* 10 60 1000))
      (printf "@@@ dr-query creating engine for curie ~s\n" curie)
      (define eng (engine (lambda (p)
                            (dr-query1 curie))))
      (printf "@@@ dr-query running engine for ~s ms for curie ~s\n" timeout-ms curie)
      (engine-run timeout-ms eng)
      (printf "@@@ dr-query engine for curie ~s finished\n" curie)
      (if (engine-result eng)
          (printf "@@@ dr-query engine for curie ~s ran to completion\n" curie)
          (printf "@@@ dr-query engine for curie ~s timed out!!\n" curie))
      (printf "@@@ dr-query killing engine for curie ~s\n" curie)
      (engine-kill eng)
      (printf "@@@ dr-query killed engine for curie ~s\n" curie)
      )
    gene-curies))

(define (tsv-for gene-curie gene-symbol infos)
  (lambda ()
    (printf "~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\n"
            "gene CURIE"
            "gene symbol"
            "db"
            "subject CURIE" "subject category" "subject"
            "predicate"
            "object" "object category" "object CURIE"
            "pub URL" "pub date" "pub sentence"
            "tradenames" "clinical trials" "indicated for" "contraindicated for")
    (for-each (lambda (xs)
                (for-each (lambda (x)
                            (apply printf "~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\n"
                                   gene-curie gene-symbol x))
                          xs))
              infos)))

#|




#|NOTES HERE|#

#|
Gives the all --predicate-->Object
(run* (p) (object-predicateo '(rtx
  1225271
  "NCIT:C102527"
  "ACE2 Gene"
  (11 . "http://w3id.org/biolink/vocab/GeneSet")
  (("iri" . "http://purl.obolibrary.org/obo/NCIT_C102527")
   ("synonym"
    .
    "['ACE2', 'Angiotensin I Converting Enzyme (Peptidyl-Dipeptidase A) 2 Gene']")
   ("category_label" . "gene_set")
   ("deprecated" . "False")
   ("description"
    .
    "This gene plays a role in both proteolysis and vasodilation.; UMLS Semantic Type: TUI:T028")
   ("provided_by" . "https://identifiers.org/umls/NCI")
   ("id" . "NCIT:C102527")
   ("update_date" . "2018")
   ("publications" . "[]")))  p))
'((rtx 15 . "subclass_of"))


(run* (p) (subject-predicateo
'(rtx
  1225271
  "NCIT:C102527"
  "ACE2 Gene"
  (11 . "http://w3id.org/biolink/vocab/GeneSet")
  (("iri" . "http://purl.obolibrary.org/obo/NCIT_C102527")
   ("synonym"
    .
    "['ACE2', 'Angiotensin I Converting Enzyme (Peptidyl-Dipeptidase A) 2 Gene']")
   ("category_label" . "gene_set")
   ("deprecated" . "False")
   ("description"
    .
    "This gene plays a role in both proteolysis and vasodilation.; UMLS Semantic Type: TUI:T028")
   ("provided_by" . "https://identifiers.org/umls/NCI")
   ("id" . "NCIT:C102527")
   ("update_date" . "2018")
   ("publications" . "[]")))  p))
'((rtx 3 . "xref")
 (rtx 15 . "subclass_of")
 (rtx 346 . "gene_encodes_gene_product")
 (rtx 354 . "gene_plays_role_in_process"))
|#


|#
|#
