#lang racket
(require racket/cmdline)
(provide (all-defined-out))
(require "query.rkt"         
         racket/engine)


(define userpath 
	(string-append
	(path->string (find-system-path 'home-dir))
	"code/repo/mediKanren/"
	)
)
(define export-path 
	(string-append
	(path->string (find-system-path 'home-dir))
	"code/repo/mediKanren/biolink/pieces-parts/results/"
	)
)
(define drug-query-path 
	(string-append
	(path->string (find-system-path 'home-dir))
	"code/repo/mediKanren/biolink/data/query_curie_lists/split/"
	)
)

(define argv (current-command-line-arguments))
(define argv-expected '#(tiny_file))
(when (not (= (vector-length argv-expected) (vector-length argv)))  (error "command line argument mismatch:" argv-expected argv))

(define data-file (vector-ref argv 0))
(define (graph-path fname) (expand-user-path (build-path data-file fname)))
(print "data-file")
(print data-file)


(define alzheimer-fda-drug-interactors
  (file->list (string-append userpath "/biolink/data/query_curie_lists/alzheimer-fda-drug-interactors.scm")))

(define top-25-alzheimer-fda-drug-interactors
  (take alzheimer-fda-drug-interactors 25))

(define alzheimer-fda-drug-upregulators
  (file->list (string-append userpath "/biolink/data/query_curie_lists/alzheimer-fda-drug-upregulators.scm")))

(define top-25-alzheimer-fda-drug-upregulators
  (take alzheimer-fda-drug-upregulators 25))

(define alzheimer-fda-drug-downregulators
  (file->list (string-append userpath "/biolink/data/query_curie_lists/alzheimer-fda-drug-downregulators.scm")))

(define top-25-alzheimer-fda-drug-downregulators
  (take alzheimer-fda-drug-downregulators 25))

#|HELPER FUNCTIONS|#
(define inner-loop/csv
  (lambda (ls port)
    (cond
      ((null? ls) (void))
      (else
       (fprintf port "~a~c" (car ls) #\,)
       (inner-loop/csv (cdr ls) port)))))

(define outer-loop/csv
  (lambda (ls port)
    (cond
      ((null? ls)
       (close-output-port port))
      (else
       (inner-loop/csv (car ls) port)
       (fprintf port (format "~c" #\newline))
       (outer-loop/csv (cdr ls) port)))))

(define export-column-headers/csv
  (lambda (headers port path)
    (cond 
      ((= (file-size path) 0)
       (cond
         ((null? headers)
          (fprintf port "~c" #\newline))
         (else
          (fprintf port "~a~c" (car headers) #\,)
          (export-column-headers/csv (cdr headers) port path))))
      (else
       (void)))))


(define export-column-headers
  (lambda (headers port path)
    (cond 
      ((= (file-size path) 0)
       (cond
         ((null? headers)
          (fprintf port "~c" #\newline))
         (else
          (fprintf port "~a~c" (car headers) #\tab)
          (export-column-headers (cdr headers) port path))))
      (else
       (void)))))

(define outer-loop
  (lambda (ls port)
    (cond
      ((null? ls)
       (close-output-port port))
      (else
       (inner-loop (car ls) port)
       (fprintf port (format "~c" #\newline))
       (outer-loop (cdr ls) port)))))

(define inner-loop
  (lambda (ls port)
    (cond
      ((null? ls) (void))
      (else
       (fprintf port "~a~c" (car ls) #\tab)
       (inner-loop (cdr ls) port)))))

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (list? x)))))

(define member?
  (lambda (x ls)
    (cond
      ((null? ls) #f)
      (else 
       (or (equal? x (car ls))
           (member? x (cdr ls)))))))

(define union
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) ls2)
      ((member? (car ls1) ls2)
       (union (cdr ls1) ls2))
      (else
       (cons (car ls1)
             (union (cdr ls1) ls2))))))

(define intersect?
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) #f)
      ((member? (car ls1) ls2) #t)
      (else
       (intersect? (cdr ls1) ls2)))))

(define intersect
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) '())
      ((member? (car ls1) ls2)
       (cons (car ls1)
             (intersect (cdr ls1) ls2)))
      (else
       (intersect (cdr ls1) ls2)))))


(define (pubmed-ids-from-edge-props eprops)
  (cond
    [(assoc "pmids" eprops) ;; WEB the 'pmids' property is only used by semmed, I believe
     => (lambda (pr) (regexp-split #rx";" (cdr pr)))]
    [(assoc "publications" eprops)
     => (lambda (pr)
          (define pubs (cdr pr))
          (if (not (string? pubs))
              '()
              (regexp-match* #rx"([0-9]+)" pubs #:match-select cadr)))]
    [else '()]))

(define (pubmed-ids-from-edge edge)
  (remove-duplicates
   (match edge
     ['path-separator '()]
     [`(,dbname ,eid ,subj ,obj ,p ,eprops)
      (pubmed-ids-from-edge-props eprops)])))
(define PUBMED_URL_PREFIX "https://www.ncbi.nlm.nih.gov/pubmed/")
(define (pubmed-URLs-from-edge edge)
  (map (lambda (pubmed-id) (string-append PUBMED_URL_PREFIX (~a pubmed-id)))
       (pubmed-ids-from-edge edge)))

(define (pubmed-count e)
  (length (pubmed-ids-from-edge e)))


#|ASSOCIATION-LIST KEYS/STRING LISTS FOR KNOWLEDGE GRAPH CONCEPTS|#
(define robokop-concept-key/eq-id "equivalent_identifiers")
(define robokop-xref-key "equivalent_identifiers")
(define orange-concept-key/synonym "synonym")
(define orange-concept-key/same-as "same_as")
(define semmed-concept-key/xrefs "xrefs")
(define semmed-concept-key/id "id")
(define publication-key "publications")
(define publication-key/pmids "pmids")
(define drug-bank/key "drugbank.groups") 
(define drug-bank/withdrawn/key "withdrawn_flag")
(define drug-bank/approved/key "drugbank.approved")
(define drug-bank/therapeutic/key "therapeutic_flag")
(define drug-bank/investigational/key "drugbank.investigational")
(define umls-type-label/key "umls_type_label")
(define umls-type/key "umls_type")
;(define PUBMED_URL_PREFIX "https://www.ncbi.nlm.nih.gov/pubmed/")

;; sans "disrupts" "treats" "affects" "augments" "associated_with"
;; "causes" "prevents" 

(define allowable/predicates-GENE-->DISEASE/MONDO-OMIM-HP-DOID-ORPHANET
  '("complicates"    
    "predisposes"
    "precedes"
    "related_to"    
    "gene_mutations_contribute_to"
    "biomarker_for"
    "contributes_to"
    "causally_related_to"
    "gene_associated_with_condition"
    "gene_associated_with_disease"
    "gene_involved_in_molecular_abnormality"
    "has_phenotype"
    "causes_condition"
    "associated_with_disease"
    "major_susceptibility_factor_in"
    "disease_causing_somatic_mutations_in"
    "disease_causing_germline_mutations_in"
    "disease_causing_germline_mutations_gain_of_function_in"
    "disease_causing_germline_mutations_loss_of_function_in"
    "modifying_germline_mutation_in"
    "candidate_gene_tested_in"
    "biomarker_tested_in"
    "INVERTED:disease_is_marked_by_gene"
    "genetic_biomarker_related_to"
    "has_disposition"
    "major_susceptibility_factor_in"
    "disease_causing_germline_mutations_in"
    "disease_causing_germline_mutations_loss_of_function_in"
    "biomarker_for"
    "biomarker_tested_in"
    "disease_causing_somatic_mutations_in"
    "disease_causing_germline_mutations_gain_of_function_in"
    "candidate_gene_tested_in"
    "modifying_germline_mutation_in"))

;; full list 
(define allowable/predicates-GENE-->DISEASE
  '("causes"
    "complicates"
    "disrupts"
    "predisposes"
    "precedes"
    "related_to"
    "prevents"
    "gene_mutations_contribute_to"
    "biomarker_for"
    "contributes_to"
    "causally_related_to"
    "gene_associated_with_condition"
    "gene_associated_with_disease"
    "gene_involved_in_molecular_abnormality"
    "has_phenotype"
    "causes_condition"
    "associated_with_disease"
    "major_susceptibility_factor_in"
    "disease_causing_somatic_mutations_in"
    "disease_causing_germline_mutations_in"
    "disease_causing_germline_mutations_gain_of_function_in"
    "disease_causing_germline_mutations_loss_of_function_in"
    "modifying_germline_mutation_in"
    "candidate_gene_tested_in"
    "biomarker_tested_in"
    "INVERTED:disease_is_marked_by_gene"
    "genetic_biomarker_related_to"
    "has_disposition"
    "major_susceptibility_factor_in"
    "disease_causing_germline_mutations_in"
    "disease_causing_germline_mutations_loss_of_function_in"
    "biomarker_for"
    "biomarker_tested_in"
    "disease_causing_somatic_mutations_in"
    "disease_causing_germline_mutations_gain_of_function_in"
    "candidate_gene_tested_in"
    "modifying_germline_mutation_in"
    "affects"
    "associated_with"
    "augments"
    "treats"))
 
(define decreases-pred-str/ls
  '("negatively_regulates"
    "negatively_regulates__entity_to_entity"
    "decreases_molecular_interaction"
    "decreases_secretion_of"
    "decreases_localization_of"
    "decreases_stability_of"
    "decreases_synthesis_of"
    "decreases_transport_of"
    "decreases_uptake_of"    
    "prevents"
    "treats"
    "disrupts"
    "inhibits"
    "inhibitor"
    "binder"
    "antagonist"
    "blocker"
    "increases_degradation_of"
    "decreases_activity_of"
    "decreases_expression_of"))

(define increases-pred-str/ls
  '("positively_regulates"
     "produces"
     "agonist"
     "causes"
     "causes_condition"
     "causally_related_to"
     "contributes_to"
     "gene_associated_with_condition"
     "positively_regulates__entity_to_entity"
     "gene_mutations_contribute_to"
     "decreases_degradation_of"
     "increases_activity_of"
     "increases_expression_of"
     "increases_molecular_interaction"
     "increases_response_to"
     "increases_secretion_of"
     "increases_stability_of"
     "increases_synthesis_of"
     "increases_transport_of"
     "increases_uptake_of"
     "increases_localization_of"
     "stimulates"
     "activator"))

(define predicate-str/ls
  '("physically_interacts_with"
    "regulates"
    "interacts_with"
    "targets"
    "modulator"
    "directly_interacts_with"
    "regulates_expression_of"
    "affects_expression_of"
    "affects"
    "affects_activity_of"
    "uses"))

(define allowable-predicates/DRUG-->GENE/GENE-->GENE
  '("negatively_regulates"
    "negatively_regulates__entity_to_entity"
    "decreases_molecular_interaction"
    "decreases_secretion_of"
    "decreases_localization_of"
    "decreases_stability_of"
    "decreases_synthesis_of"
    "decreases_transport_of"
    "decreases_uptake_of"    
    "prevents"
    "treats"
    "disrupts"
    "inhibits"
    "inhibitor"
    "binder"
    "antagonist"
    "blocker"
    "increases_degradation_of"
    "decreases_activity_of"
    "decreases_expression_of"
    "positively_regulates"
    "produces"
    "agonist"
    "causes"
    "causes_condition"
    "causally_related_to"
    "contributes_to"
    "gene_associated_with_condition"
    "positively_regulates__entity_to_entity"
    "gene_mutations_contribute_to"
    "decreases_degradation_of"
    "increases_activity_of"
    "increases_expression_of"
    "increases_molecular_interaction"
    "increases_response_to"
    "increases_secretion_of"
    "increases_stability_of"
    "increases_synthesis_of"
    "increases_transport_of"
    "increases_uptake_of"
    "increases_localization_of"
    "stimulates"
    "activator"
    "physically_interacts_with"
    "regulates"
    "interacts_with"
    "targets"
    "modulator"
    "directly_interacts_with"
    "regulates_expression_of"
    "affects_expression_of"
    "affects"
    "affects_activity_of"
    "uses"))

(define substitute
  (lambda (ls old new)
    (cond 
      ((null? ls) '())
      ((void? (car ls))
       (substitute (cdr ls) old new))
      ((boolean? (car ls))
       (cons
         (format "~a" (car ls))
         (substitute (cdr ls) old new))) 
      ((equal? (car ls) old)
       (cons new
             (substitute (cdr ls) old new)))
      (else
       (cons (car ls)
             (substitute (cdr ls) old new))))))

(define str-converter
  (lambda (ls)
    (cond
      ((null? ls)
       (substitute ls '() "NA"))
      ((or (boolean? (car ls))
           (void? (car ls)))
       (str-converter (cdr ls)))
      (else 
       (if (symbol? (car ls))
           (string-join (map symbol->string ls) " ")
           (string-join ls " "))))))

(define append-predicate-symbol
  (lambda (pred-str inc-pred dec-pred els)
    (cond
      ((null? pred-str) (car els))
      ((or (boolean? (car pred-str))
           (void? (car pred-str)))
       (append-predicate-symbol
        (cdr pred-str) inc-pred dec-pred els))
      ((member? (car pred-str) inc-pred)
       (append-predicate-symbol
        (cdr pred-str)
        inc-pred
        dec-pred
        (cons
         "1" els)))
      ((member? (car pred-str) dec-pred)
        (append-predicate-symbol
         (cdr pred-str)
         inc-pred
         dec-pred
         (cons
          "-1" els)))
      ((not (member? (car pred-str) dec-pred))
       (append-predicate-symbol
        (cdr pred-str)
         inc-pred
         dec-pred
         (cons
          "0" els)))
      (else
       (error (format "PREDICATE NOT A MEMBER OF GLOBAL VARIABLE:\n ~a" (car pred-str)))))))

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


(define get-xrefs 
  (lambda (key properties-ls)
    (cond 
      [(assoc key properties-ls)
       => (lambda (assoc-ls)
            (define x-refs (cdr assoc-ls))
            (define in (open-input-string x-refs))
            (cons (read in) '()))]
      (else
       '()))))

(define get-assoc-value
  (lambda (key assoc-ls)
    (cond
      ((assoc key assoc-ls)
       => (lambda (assoc-ls)
            (let ((assoc-key (car assoc-ls))
                  (assoc-value (cdr assoc-ls)))
              (if (equal? assoc-key key)
                  assoc-value
                  "NA"))))
      (else
       '()))))


(define get-RXNORM-from-synonyms-ls
  (lambda (curie)
    (cond      
      ((null? curie)
       "No-RXNORM-ID")
      (else
       (let ((synonyms-ls (flatten (set->list (curie-synonyms curie)))))
         (let loop ((synonyms-ls synonyms-ls))
           (cond
             ((null? synonyms-ls) "No-RXNORM-ID")
             ((string-prefix? (car synonyms-ls) "RXNORM:")
              (car synonyms-ls))
             (else
              (loop (cdr synonyms-ls))))))))))


(define gene/protein-concept-in-synonyms-ls?
  (lambda (synonyms-ls)
    (cond      
      ((null? synonyms-ls) #f)
      (else
       (or (string-prefix? (car synonyms-ls) "HGNC:")
           (string-prefix? (car synonyms-ls) "NCBIGene:")
           (string-prefix? (car synonyms-ls) "ENSEMBL:")
           (string-prefix? (car synonyms-ls) "UniProtKB:")
           (gene/protein-concept-in-synonyms-ls? (cdr synonyms-ls)))))))

(define get-gene/protein-concept-from-synonyms-ls
  (lambda (synonyms-ls els)
    (cond      
      ((null? synonyms-ls)
       (cons "No-Preferred-ID" els))
      ((string-prefix? (car synonyms-ls) "HGNC:")
       (cons (car synonyms-ls) els))
      (else
       (get-gene/protein-concept-from-synonyms-ls (cdr synonyms-ls) els)))))

(define drug-concept-in-synonyms-ls?
  (lambda (synonyms-ls)
    (cond      
      ((null? synonyms-ls) #f)
      (else
       (or (string-prefix? (car synonyms-ls) "CHEBI:")
           (string-prefix? (car synonyms-ls) "DRUGBANK:")
           (string-prefix? (car synonyms-ls) "PUBCHEM:")
           (string-prefix? (car synonyms-ls) "CHEMBL")
           (string-prefix? (car synonyms-ls) "RXNORM:")
           (drug-concept-in-synonyms-ls? (cdr synonyms-ls)))))))

(define get-drug-concept-from-synonyms-ls
  (lambda (synonyms-ls els)
    (cond      
      ((null? synonyms-ls)
       (cons "No-Preferred-ID" els))
      ((string-prefix? (car synonyms-ls) "DRUGBANK:")
       (cons (car synonyms-ls) els))
      ((string-prefix? (car synonyms-ls) "CHEBI:")
       (cons (car synonyms-ls) els))      
      ((string-prefix? (car synonyms-ls) "PUBCHEM:")
       (cons (car synonyms-ls) els))
      ((string-prefix? (car synonyms-ls) "CHEMBL")
       (cons (car synonyms-ls) els))
      ((string-prefix? (car synonyms-ls) "RXNORM:")
       (cons (car synonyms-ls) els))
      (else
       (get-drug-concept-from-synonyms-ls (cdr synonyms-ls) els)))))

;; using OMIM for gene-to-disease associations
(define disease-concept-in-synonyms-ls?
  (lambda (synonyms-ls)
    (cond      
      ((null? synonyms-ls) #f)
      (else
       (or (string-prefix? (car synonyms-ls) "MONDO:")
           (string-prefix? (car synonyms-ls) "HP:")
           (string-prefix? (car synonyms-ls) "DOID:")
           (string-prefix? (car synonyms-ls) "OMIM:")
           (disease-concept-in-synonyms-ls? (cdr synonyms-ls)))))))

(define get-disease-concept-from-synonyms-ls
  (lambda (synonyms-ls els)
    (cond      
      ((null? synonyms-ls)
       (cons "No-Preferred-ID" els))
      ((string-prefix? (car synonyms-ls) "MONDO:")
       (cons (car synonyms-ls) els))
      ((string-prefix? (car synonyms-ls) "HP:")
       (cons (car synonyms-ls) els))
      ((string-prefix? (car synonyms-ls) "DOID:")
       (cons (car synonyms-ls) els))
      ((string-prefix? (car synonyms-ls) "OMIM:")
       (cons (car synonyms-ls) els))      
      (else
       (get-disease-concept-from-synonyms-ls (cdr synonyms-ls) els)))))

(define get-preferred-curie-from-edge-subject/object
  (lambda (subject/object-from-edge)
    (cond
      ((string-contains? subject/object-from-edge ":")
       (let ((synonyms-ls (set->list (curie-synonyms subject/object-from-edge))))
         (cond
           ((gene/protein-concept-in-synonyms-ls? synonyms-ls)
            (get-gene/protein-concept-from-synonyms-ls synonyms-ls '()))
           ((drug-concept-in-synonyms-ls? synonyms-ls) 
            (get-drug-concept-from-synonyms-ls synonyms-ls '()))
           ((disease-concept-in-synonyms-ls? synonyms-ls) 
            (get-disease-concept-from-synonyms-ls synonyms-ls '()))
           (else
            "No-Preferred-ID"))))
      (else
       "No-Preferred-ID"))))


(define get-preferred-string-name-from-edge-subject/object
  (lambda (subject/object-from-edge)
    (cond
      ((or (null? subject/object-from-edge)
           (boolean? subject/object-from-edge)
           (null? (find-concepts #t (set->list (curie-synonyms subject/object-from-edge)))))
       "No-Preferred-String-Name")
      (else
       (let ((concept-ls (find-concepts #t (set->list (curie-synonyms subject/object-from-edge)))))
         (let loop ((concept-ls concept-ls))
           (cond
             ((null? concept-ls)
              "No-Preferred-String-Name")
             ((and (equal? (concept->dbname (car concept-ls)) 'robokop)
                        (string-contains? (concept->curie (car concept-ls)) "HGNC:"))
                   (concept->name (car concept-ls)))
             ((and (equal? (concept->dbname (car concept-ls)) 'orange)
                   (string-contains? (concept->curie (car concept-ls)) "NCBIGene:"))
              (concept->name (car concept-ls)))
             ((and (equal? (concept->dbname (car concept-ls)) 'rtx_2020)
                 (string-contains? (concept->curie (car concept-ls)) "UniProtKB:"))
              (concept->name (car concept-ls)))                         
             ((and (equal? (concept->dbname (car concept-ls)) 'rtx_2020)
                   (or (string-contains? (concept->curie (car concept-ls)) "DRUGBANK:")))
              (concept->name (car concept-ls)))
             ((and (equal? (concept->dbname (car concept-ls)) 'orange)
                   (string-contains? (concept->curie (car concept-ls)) "MONDO:"))
              (concept->name (car concept-ls)))
             (else
              (loop (cdr concept-ls))))))))))

(define get-pubmed-info-from-edge-ls
  (lambda (edge-ls)
    (append* (map (lambda (edge)
                 (define pub-info (publications-info-alist-from-edge edge))
                 (define pub-urls (pubmed-URLs-from-edge edge))
                 (match edge
                   [`(,db
                       ,edge-id
                       (,_ ,subject-curie ,subject-name (,_ . ,subject-cat) . ,_)
                       (,_ ,object-curie ,object-name (,_ . ,object-cat) . ,_)
                       (,_ . ,predicate) . ,_)
                     (define (entry pubmed-url pub-date sentence)
                       (list edge-id db
                               subject-curie subject-name subject-cat
                               predicate
                               object-curie  object-name object-cat
                               pubmed-url pub-date sentence))
                     (cond ((pair? pub-info)
                            (map (lambda (info)
                                   (match info
                                     [`(,pubmed-url ,pub-date ,subject-score ,object-score ,sentence)
                                       (entry pubmed-url pub-date sentence)]))
                                 pub-info))
                           ((pair? pub-urls)
                            (map (lambda (url) (entry url "" "")) pub-urls))
                           (else (list (entry "" "" ""))))]))
         edge-ls))))



;;11 items
(define append-preferred-curie/string-names-to-export-edges
  (lambda (edges-ls)
    (cond
      ((null? edges-ls) '())
      (else
       (match (car edges-ls)                       
         [`(,edge-id ,db ,subject-id ,subject-name ,subject-cat ,pred ,object-id ,object-name ,object-cat ,pubmed-url ,pub-date ,sentence)
          (cons
           (substitute
            (flatten
            (list
             (get-preferred-string-name-from-edge-subject/object subject-id)
             (append-predicate-symbol (list pred) increases-pred-str/ls decreases-pred-str/ls '())
             (get-preferred-string-name-from-edge-subject/object object-id)
             pub-date
             (string-trim pubmed-url "https://www.ncbi.nlm.nih.gov/pubmed/")             
             sentence
             "meta-data"
             edge-id
             db
             subject-id
             (get-preferred-curie-from-edge-subject/object subject-id)
             subject-name
             subject-cat
             pred
             object-id
             (get-preferred-curie-from-edge-subject/object object-id)
             object-name
             object-cat
             (get-RXNORM-from-synonyms-ls subject-id)
             (get-RXNORM-from-synonyms-ls object-id)))
            '()
            "NA"
            )           
           (append-preferred-curie/string-names-to-export-edges          
            (cdr edges-ls)))])))))


(define column-headers/1hop
  '("subject_preferred_name"
    "predicate_symbol"
    "object_preferred_name"
    "publication_date"
    "pubmed_id"
    "nlp_evidence"
    "meta_data_break"
    "edge_id"
    "db"
    "subject_curie"              
    "subject_preferred_curie"
    "subject_name"
    "subject_category"
    "predicate_name"
    "object_curie"
    "object_preferred_curie"
    "object_name"
    "object_category"
    "subject_fda_approval"
    "object_fda_approval"))

(define column-headers/2hop
  '("X2_subject_preferred_name"
    "X2predicate_symbol"
    "X2_object_preferred_name"
    "X1_predicate"
    "target_concept"
    "publication_date"
    "pubmed_id"
    "nlp_evidence"
    "meta_data_break"
    "edge_id"
    "db"
    "subject_curie"              
    "subject_preferred_curie"
    "subject_name"
    "subject_category"
    "predicate_name"
    "object_curie"
    "object_preferred_curie"
    "object_name"
    "object_category"
    "subject_fda_approval"
    "object_fda_approval"))

#|
#|sample new query graph|#
;; (publications-info-alist-from-edge edge)
(define ((edge/db? db) e) (eq? db (car e)))

(define q (time (query/graph
                  ((X       drug)
                   (Y       gene-or-protein)
                   (rhobtb2 "UMLS:C1425762"))
                  ((X->Y       negatively-regulates (edge/db? 'robokop))
                   ;(X->Y       negatively-regulates (edge/db? 'semmed))
                   (Y->rhobtb2 positively-regulates (edge/db? 'uab-pmi)))
                  (X X->Y Y Y->rhobtb2 rhobtb2))))
|#

#|
(define QUERY:X--Predicate-->Target-Concept
  (lambda (target-concept-ls X-concept-filter-ls predicates-ls db)
    (define 1-hop/query
      (lambda (target-concept)
        (printf "\nQUERY/GRAPH RUNNING ON: ~a\n" target-concept)
        (time
         (query/graph
                 ((X X-concept-filter-ls)
                  (TC target-concept))
                 ((X->TC predicates-ls) (edge/db? db))
                 (X X->TC TC)))))
    (cond
      ((null? target-concept-ls) '())
      (else
       (append*
        (edges/query (1-hop/query (car target-concept-ls)) 'X->TC)
        (QUERY:X--Predicate-->Target-Concept
         (cdr target-concept-ls)
         X-concept-filter-ls
         predicates-ls
         db))))))
|#



(define QUERY:X--Predicate-->Target-Concept  
  (lambda
      (
       concept-ls
       x-filter
       p-filter
       db-filter
       )
    (define 1-hop/query
      (lambda
          (
           concept
           x-filter
           p-filter
           db-filter
           )
        (printf "\nQUERY/GRAPH RUNNING ON: ~a\n" concept)
        (time         
         (query/graph
          ((X x-filter)
           (TC concept))
          ((X->TC p-filter) (edge/db? db-filter))
          (X X->TC TC)))))
    (cond
      ((null? concept-ls) '())
      (else
       (let* ((1-hop/query-result (1-hop/query
                                   (car concept-ls)
                                   x-filter
                                   p-filter
                                   db-filter
                                   ))
              (1hop-edge-ls
               (edges/query
                1-hop/query-result
                'X->TC
                )))         
         (append*
          1hop-edge-ls
          (QUERY:X--Predicate-->Target-Concept
           (cdr concept-ls)
           x-filter
           p-filter
           db-filter
           )))))))



(define QUERY:Target-Concept--Predicate-->X
  (lambda
      (
       concept-ls
       x-filter
       p-filter
       db-filter
       )
    (define 1-hop/query
      (lambda
          (
           concept
           x-filter
           p-filter
           db-filter
           )
        (printf "\nQUERY/GRAPH RUNNING ON: ~a\n" concept)
        (time         
         (query/graph
          ((X x-filter)
           (TC concept))
          ((TC->X p-filter) (edge/db? db-filter))
          (TC TC->X X)))))
    (cond
      ((null? concept-ls) '())
      (else
       (let* ((1-hop/query-result (1-hop/query
                                   (car concept-ls)
                                   x-filter
                                   p-filter
                                   db-filter
                                   ))
              (1hop-edge-ls
               (edges/query
                1-hop/query-result
                'TC->X
                )))         
         (append*
          1hop-edge-ls
          (QUERY:Target-Concept--Predicate-->X
           (cdr concept-ls)
           x-filter
           p-filter
           db-filter
           )))))))




(define gene-concept?
  (lambda (x)
    (or
     (string-prefix? x "HGNC:")
     (string-prefix? x "ENSEMBL:")
     (string-prefix? x "UniProtKB:")
     (string-prefix? x "NCBIGene:")
     (string-prefix? x "NCBIGENE:"))))

(define drug-concept?
  (lambda (x)
    (or (string-prefix? x "CHEBI:")
        (string-prefix? x "CHEMBL:")
        (string-prefix? x "CHEMBL.")
        (string-prefix? x "KEGG:")
        (string-prefix? x "KEGG.")
        (string-prefix? x "DRUGBANK:")
        (string-prefix? x "RXNORM:"))))

(define disease-concept?
  (lambda (x)
    (or (string-prefix? x "OMIM:")
        (string-prefix? x "DOID:")
        (string-prefix? x "MONDO:")
        (string-prefix? x "HP:"))))

(define lat?
  (lambda (ls)
    (cond
      ((null? ls) #t)
      ((atom? (car ls))
       (lat? (cdr ls)))
      (else
       #f))))

(define get-export-edges
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((lat? (car ls))
       (cons (car ls)
             (get-export-edges (cdr ls))))
      (else
       (set-union (get-export-edges (car ls))
                  (get-export-edges (cdr ls)))))))



(define drug->gene/predicates
  '("physically_interacts_with" "directly_interacts_with" "targets" 
    "biolink:molecularly_interacts_with" "biolink:interacts_with" "biolink:related_to" 
    "negatively_regulates"
    "negatively_regulates__entity_to_entity"
    "decreases_molecular_interaction"
    "decreases_secretion_of"
    "decreases_localization_of"
    "decreases_stability_of"
    "decreases_synthesis_of"
    "decreases_transport_of"
    "decreases_uptake_of"    
    "disrupts"
    "inhibits"
    "inhibitor"
    "binder"
    "targets"
    "antagonist"
    "blocker"
    "increases_degradation_of"
    "decreases_activity_of"
    "decreases_expression_of"
    "positively_regulates"
    "produces"
    "agonist"
    "positively_regulates__entity_to_entity"
    "decreases_degradation_of"
    "increases_activity_of"
    "increases_expression_of"
    "increases_molecular_interaction"
    "increases_response_to"
    "increases_secretion_of"
    "increases_stability_of"
    "increases_synthesis_of"
    "increases_transport_of"
    "increases_uptake_of"
    "increases_localization_of"
    "stimulates"
    "activator"
    "physically_interacts_with"
    "regulates"
    "interacts_with"
    "targets"
    "modulator"
    "directly_interacts_with"
    "regulates_expression_of"
    "affects_expression_of"
    "affects"
    "affects_activity_of"
    "uses"
    ))


#;(define start-function/TARGET-CONCEPT--PREDICATE-->X
  (lambda (curie-ls query-ls-name concept-filter predicate-filter db)
    (cond
      ((null? curie-ls) (void))
      (else
       (HANDLE-QUERY:TARGET-CONCEPT--PREDICATE-->X (car curie-ls) query-ls-name concept-filter predicate-filter db)
       (start-function/TARGET-CONCEPT--PREDICATE-->X (cdr curie-ls) query-ls-name concept-filter predicate-filter db)))))


(define start-function/X--PREDICATE-->TARGET-CONCEPT
  (lambda (curie-ls query-ls-name concept-filter predicate-filter db)
    (cond
      ((null? curie-ls) (void))
      (else
       (HANDLE-QUERY:X--PREDICATE-->TARGET-CONCEPT (car curie-ls) query-ls-name concept-filter predicate-filter db)
       (start-function/X--PREDICATE-->TARGET-CONCEPT (cdr curie-ls) query-ls-name concept-filter predicate-filter db)))))

(define start-function/TARGET-CONCEPT--PREDICATE-->X
  (lambda (curie-ls query-ls-name concept-filter predicate-filter db)
    (cond
      ((null? curie-ls) (void))
      (else
       (HANDLE-QUERY:TARGET-CONCEPT--PREDICATE-->X (car curie-ls) query-ls-name concept-filter predicate-filter db)
       (start-function/TARGET-CONCEPT--PREDICATE-->X (cdr curie-ls) query-ls-name concept-filter predicate-filter db)))))



;;(start-function anti-coag-drug-curies gene-ls)
(define export-date
  (format "~a_~a_~a" 
          (number->string (date-month (seconds->date (current-seconds))))
          (number->string (date-day (seconds->date (current-seconds))))
          (number->string (date-year (seconds->date (current-seconds))))))


;;(define export-path
;;  (format "~a" (path->string (find-system-path 'desk-dir))))

(define directory-path 
  (format "~a~a/" export-path export-date))

(define HANDLE-QUERY:X--PREDICATE-->TARGET-CONCEPT
  (time
   (lambda (curie
            query-ls-name
            concept-filter
            predicate-filter
            db)
     (cond
       ((null? curie) (void))
       (else                  
        (define Q-RESULT:X--Predicate-->Target-Concept/path
          (format "~aALLc--DRUG-PREDICATES-->~a.tsv" export-path query-ls-name))               
        (define Q-RESULT:X--Predicate-->Target-Concept/port
          (open-output-file Q-RESULT:X--Predicate-->Target-Concept/path #:exists 'append))
        
        (define Q-RESULT:X--Predicate-->Target-Concept  
          (append-preferred-curie/string-names-to-export-edges
           (get-pubmed-info-from-edge-ls
            (remove-duplicates
             (QUERY:X--Predicate-->Target-Concept
              (list curie)
              concept-filter
              predicate-filter
              db)))))

        (export-column-headers
         column-headers/1hop
          Q-RESULT:X--Predicate-->Target-Concept/port
         Q-RESULT:X--Predicate-->Target-Concept/path)
          
        (outer-loop
         Q-RESULT:X--Predicate-->Target-Concept
          Q-RESULT:X--Predicate-->Target-Concept/port)

        )))))

#|
(start-function/X--PREDICATE-->TARGET-CONCEPT
 alzheimer-gene-curies
 "alzheimer-gene-curies"
 drug-concept?
 #f
 #f)
|#

(define HANDLE-QUERY:TARGET-CONCEPT--PREDICATE-->X
  (time
   (lambda (curie
            query-ls-name
            concept-filter
            predicate-filter
            db)
     (cond
       ((null? curie) (void))
       (else                  
        (define Q-RESULT:Target-Concept--Predicate-->X/path
          (format "~a~a--ALLp-->GENE-CONCEPTS.tsv" export-path query-ls-name))               
        (define Q-RESULT:Target-Concept--Predicate-->X/port
          (open-output-file Q-RESULT:Target-Concept--Predicate-->X/path #:exists 'append))
        
        (define Q-RESULT:Target-Concept--Predicate-->X
          (append-preferred-curie/string-names-to-export-edges
           (get-pubmed-info-from-edge-ls
            (remove-duplicates
             (QUERY:Target-Concept--Predicate-->X
              (list curie)
              concept-filter
              predicate-filter
              db)))))

        (export-column-headers
         column-headers/1hop
          Q-RESULT:Target-Concept--Predicate-->X/port          
          Q-RESULT:Target-Concept--Predicate-->X/path
          )
          
        (outer-loop
         Q-RESULT:Target-Concept--Predicate-->X
         Q-RESULT:Target-Concept--Predicate-->X/port
         )
        
        )))))

(define decreases-drug-repurposing-predicate
  '("negatively_regulates"
    "negatively_regulates__entity_to_entity"
    "decreases_molecular_interaction"
    "decreases_secretion_of"
    "decreases_localization_of"
    "decreases_stability_of"
    "decreases_synthesis_of"
    "decreases_transport_of"
    "decreases_uptake_of"
    "prevents"
    "treats"
    "disrupts"
    "inhibits"
    "inhibitor"
    "binder"
    "antagonist"
    "blocker"
    "increases_degradation_of"
    "decreases_activity_of"
    "decreases_expression_of"
    ))


(start-function/TARGET-CONCEPT--PREDICATE-->X

(file->list (string-append drug-query-path data-file))

;; "alzheimer-gene-downregulator-drugs"
 (string-append "down_results_" data-file)
 gene/protein-concept?
 decreases-drug-repurposing-predicate
 #f)

(define increases-expression-drug-repurposing
  '("positively_regulates"
    "produces"
    "agonist"
    "causes"
    "causes_condition"
    "causally_related_to"
    "contributes_to"
    "gene_associated_with_condition"
    "positively_regulates__entity_to_entity"
    "gene_mutations_contribute_to"
    "decreases_degradation_of"
    "increases_activity_of"
    "increases_expression_of"
    "increases_molecular_interaction"
    "increases_response_to"
    "increases_secretion_of"
    "increases_stability_of"
    "increases_synthesis_of"
    "increases_transport_of"
    "increases_uptake_of"
    "increases_localization_of"
    "stimulates"
    "activator"
    ))


(start-function/TARGET-CONCEPT--PREDICATE-->X
(file->list (string-append drug-query-path data-file))
 (string-append "up_results_" data-file)
 ;;alzheimer-fda-drug-upregulators
 ;;"alzheimer-gene-upregulator-drugs"
 gene/protein-concept?
 increases-expression-drug-repurposing
 #f)

(define interacts-with-drug-repurposing-predicate
  '("interacts_with" "directly_interacts_with" "moleculary_interacts_with"))

(start-function/TARGET-CONCEPT--PREDICATE-->X
;; alzheimer-fda-drug-interactors
;; "alzheimer-gene-interactor-drugs"
(file->list (string-append drug-query-path data-file))
 (string-append "interact_results_" data-file)
 gene/protein-concept?
 interacts-with-drug-repurposing-predicate
 #f)



#|

(start-function/X--PREDICATE-->TARGET-CONCEPT
 alzheimer-gene-curies
 "alzheimer-gene-curies"
 drug-concept?
 #f
 #f)
|#
