#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"
 json
 racket/format
 racket/list
 racket/match
 racket/set
 racket/pretty
 racket/string)

(define robokop-top-bucket (list 5)) ;top/max bucket num of RoboKop KG
(define text-mining-top-bucket (list 5)) ;top/max bucket num of Text Mining KG
(define rtx-kg2-top-bucket (list 7)) ;top/max bucket num of RTX-KG2 KG

; Numbers of the top buckets of RoboKop KG, Text Mining KG, and RTX-KG2 KG (in this order).
; [The higer the bucket number, the higher amount of publications supporting the edge]
(define TOP_BUCKET_NUMBERS (list robokop-top-bucket 
                                 text-mining-top-bucket 
                                 rtx-kg2-top-bucket 
                                 ))

(define regulates-EGFR
  (time (query:X->Known
         #f
         (set->list
          (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
           '("biolink:regulates")))
         (set->list
          (get-descendent-curies*-in-db
           (curies->synonyms-in-db (list "HGNC:3236")))))))

(define regulates-EGFR-faster
  (time (query:X->Known-scored
         #f
         (set->list
          (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
           '("biolink:regulates")))
         (set->list
          (get-descendent-curies*-in-db
           (curies->synonyms-in-db (list "HGNC:3236"))))
         TOP_BUCKET_NUMBERS
         )))
;; There is a problem when we use the bucketing approach is that we might 
;; receive zero answer from the buckets that has a higer amount of supports.
;; Hence, you can either manually decrease the bucket number and redo the
;; query, or you may write a procedure to realize auto growing until reach
;; the amount of answers you want.
;; `auto-grow` used by mediKanren neo server:
;; https://github.com/webyrd/mediKanren/blob/master/medikanren2/neo/neo-server/neo-server-utils.rkt#L80-L92

; If I would like to start with 2 buckets from, for example, rtx-kg2:
; [we can have 1 or 1 more bucker number as a list in each of the
; element of the bukect list]
(define regulates-EGFR-faster-2bks-from-kg2
  (time (query:X->Known-scored
         #f
         (set->list
          (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
           '("biolink:regulates")))
         (set->list
          (get-descendent-curies*-in-db
           (curies->synonyms-in-db (list "HGNC:3236"))))
         (list (list 5) (list 5) (list 6 7))
         )))

; Or we do not want the top bucket at all, for example, from robokop KG:
(define regulates-EGFR-faster-notop-robokop
  (time (query:X->Known-scored
         #f
         (set->list
          (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
           '("biolink:regulates")))
         (set->list
          (get-descendent-curies*-in-db
           (curies->synonyms-in-db (list "HGNC:3236"))))
         (list (list 4) ;; robokop KG has the max bucket number 5
               (list 5) (list 7))
         )))

; If I only want to get answers from, for example, the text mining KG:
; [#f in the correspong index of the bucket list means not do query in
; this specific KG]
(define regulates-EGFR-faster-only-tm
  (time (query:X->Known-scored
         #f
         (set->list
          (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
           '("biolink:regulates")))
         (set->list
          (get-descendent-curies*-in-db
           (curies->synonyms-in-db (list "HGNC:3236"))))
         (list #f (list 5) #f)
         )))

(define downregulates-EGFR
  (filter
   (lambda (e)
     (match e
       [`(,subj ,pred ,obj . ,props)
        (and (member '("predicate" "biolink:regulates") props)
             (member '("object_direction_qualifier" "downregulated") props))]))
   (query:X->Known
    #f
    (set->list
     (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
      '("biolink:affects")))
    (set->list
     (get-descendent-curies*-in-db
      (curies->synonyms-in-db (list "HGNC:3236")))))))

(define diabetes-causes
  (query:X->Known
   #f
   (set->list
    (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
     '("biolink:causes")))
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db
      (list "DOID:9351"))))))

(define diabetes-treatments
  (query:X->Known
   #f
   (set->list
    (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
     '("biolink:treats")))
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db
      (list "DOID:9351"))))))

(curies->synonyms-in-db (list "PUBCHEM.COMPOUND:5291"))
;; =>
'("PUBCHEM.COMPOUND:5291"
  "PUBCHEM.COMPOUND:123596"
  "DRUGBANK:DB00619")

(define imatinib-regulates
  (time (query:Known->X
         (set->list
          (get-descendent-curies*-in-db
           (curies->synonyms-in-db (list "PUBCHEM.COMPOUND:5291"))))
         (set->list
          (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
           '("biolink:regulates")))
         #f)))

(define imatinib-regulates-faster
  (time (query:Known->X-scored
         (set->list
          (get-descendent-curies*-in-db
           (curies->synonyms-in-db (list "PUBCHEM.COMPOUND:5291"))))
         (set->list
          (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
           '("biolink:regulates")))
         #f
         TOP_BUCKET_NUMBERS)))

; `query:Known->Known` has not support the faster version yet
(define BCL2-affects-diabetes
  (query:Known->Known
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db
      (list "HGNC:990"))))
   (set->list
    (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
     '("biolink:affects")))
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db
      (list "DOID:9351"))))))

; --------------------

(define Casein-Kinase-2-Alpha-1
  (list "HGNC:2457"
        "NCBIGene:1457"
        "ENSEMBL:ENSG00000101266"
        "OMIM:115440"
        "UniProtKB:P68400"))

(define Casein-Kinase-2-Alpha-1+
  (remove-duplicates (curies-in-db (curies->synonyms Casein-Kinase-2-Alpha-1))))

(define (gene*->protein* gene*)
  (remove-duplicates
   (map car
        (query:X->Known-scored 
         (set->list
          (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
           '("biolink:Protein")))
         #;'("biolink:Protein")
         '("biolink:gene_product_of")
         gene*
         (list #f #f (list 0))))))

(define Casein-Kinase-2-Alpha-1-protein (gene*->protein* Casein-Kinase-2-Alpha-1+))

(define chem-regulate-Casein-Kinase-2-Alpha-1
  (time (query:X->Known
   (set->list
    (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
     '("biolink:ChemicalEntity")))
   (set->list
    (get-non-deprecated/mixin/absreact-ins-and-descendent-predicates*-in-db
     '("biolink:affects")))
   (remove-duplicates (append Casein-Kinase-2-Alpha-1-protein Casein-Kinase-2-Alpha-1+)))))

;; To see the predicates from the answers
(remove-duplicates (map cadr chem-regulate-Casein-Kinase-2-Alpha-1))

(define chem-up-regulates-Casein-Kinase-2-Alpha-1
  (filter
   (lambda (e)
     (match e
       [`(,subj ,pred ,obj . ,props)
        (or (member '("object_direction_qualifier" "increased") props)
             (member '("object_direction_qualifier" "upregulated") props))]))
   chem-regulate-Casein-Kinase-2-Alpha-1))
#|
'(("PUBCHEM.COMPOUND:15625"
   "biolink:affects"
   "NCBIGene:1457"
   ("NCBITaxon" "10090")
   ("biolink:primary_knowledge_source" "infores:ctd")
   ("description" "increases molecular interaction with")
   ("object" "NCBIGene:1457")
   ("object_aspect_qualifier" "molecular_interaction")
   ("object_direction_qualifier" "increased")
   ("predicate" "biolink:affects")
   ("publications" "(PMID:19654925)")
   ("qualified_predicate" "biolink:causes")
   ("subject" "PUBCHEM.COMPOUND:15625"))
  ("PUBCHEM.COMPOUND:15625"
   "biolink:affects"
   "NCBIGene:1457"
   ("NCBITaxon" "10090")
   ("biolink:primary_knowledge_source" "infores:ctd")
   ("description" "increases molecular interaction with")
   ("object" "NCBIGene:1457")
   ("object_aspect_qualifier" "molecular_interaction")
   ("object_direction_qualifier" "increased")
   ("predicate" "biolink:affects")
   ("publications" "(PMID:19654925)")
   ("qualified_predicate" "biolink:causes")
   ("subject" "PUBCHEM.COMPOUND:15625"))
  ("PUBCHEM.COMPOUND:15625"
   "biolink:affects"
   "NCBIGene:1457"
   ("NCBITaxon" "10090")
   ("biolink:primary_knowledge_source" "infores:ctd")
   ("description" "increases molecular interaction with")
   ("object" "NCBIGene:1457")
   ("object_aspect_qualifier" "molecular_interaction")
   ("object_direction_qualifier" "increased")
   ("predicate" "biolink:affects")
   ("publications" "(PMID:19654925)")
   ("qualified_predicate" "biolink:causes")
   ("subject" "PUBCHEM.COMPOUND:15625"))
  ("PUBCHEM.COMPOUND:5991"
   "biolink:affects"
   "NCBIGene:1457"
   ("NCBITaxon" "10090")
   ("biolink:primary_knowledge_source" "infores:ctd")
   ("description" "increases expression of")
   ("object" "NCBIGene:1457")
   ("object_aspect_qualifier" "expression")
   ("object_direction_qualifier" "increased")
   ("predicate" "biolink:affects")
   ("publications" "(PMID:17942748 PMID:29097150 PMID:24096037)")
   ("qualified_predicate" "biolink:causes")
   ("subject" "PUBCHEM.COMPOUND:5991"))
  ("PUBCHEM.COMPOUND:5991"
   "biolink:affects"
   "NCBIGene:1457"
   ("NCBITaxon" "10090")
   ("biolink:primary_knowledge_source" "infores:ctd")
   ("description" "increases expression of")
   ("object" "NCBIGene:1457")
   ("object_aspect_qualifier" "expression")
   ("object_direction_qualifier" "increased")
   ("predicate" "biolink:affects")
   ("publications" "(PMID:17942748 PMID:29097150 PMID:24096037)")
   ("qualified_predicate" "biolink:causes")
   ("subject" "PUBCHEM.COMPOUND:5991"))
  ("PUBCHEM.COMPOUND:5991"
   "biolink:affects"
   "NCBIGene:1457"
   ("NCBITaxon" "10090")
   ("biolink:primary_knowledge_source" "infores:ctd")
   ("description" "increases expression of")
   ("object" "NCBIGene:1457")
   ("object_aspect_qualifier" "expression")
   ("object_direction_qualifier" "increased")
   ("predicate" "biolink:affects")
   ("publications" "(PMID:17942748 PMID:29097150 PMID:24096037)")
   ("qualified_predicate" "biolink:causes")
   ("subject" "PUBCHEM.COMPOUND:5991"))
  ("DRUGBANK:DB03088"
   "biolink:affects"
   "UniProtKB:P68400"
   ("assertion_id"
    "128b76f85670ba85bb5b91cad9758237cc0886ac449739de46d906886b198350")
   ("association_curie" "biolink:ChemicalToGeneAssociation")
   ("json_attributes"
    "[{\"attribute_type_id\": \"biolink:primary_knowledge_source\", \"value\": \"infores:text-mining-provider-targeted\", \"value_type_id\": \"biolink:InformationResource\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:supporting_data_source\", \"value\": \"infores:pubmed\", \"value_type_id\": \"biolink:InformationResource\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:evidence_count\", \"value\": 1, \"value_type_id\": \"biolink:EvidenceCount\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:extraction_confidence_score\", \"value\": 0.9163411100614833, \"value_type_id\": \"biolink:ConfidenceLevel\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:publications\", \"value\": [\"PMC:4631799\"], \"value_type_id\": \"biolink:Uriorcurie\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:has_supporting_study_result\", \"value\": \"tmkp:480d75d32bc72138b0593be1e14c0f0087ec2cc41b61f80110402e8ff7f34748\", \"value_type_id\": \"biolink:TextMiningResult\", \"value_url\": \"https://tmui.text-mining-kp.org/evidence/480d75d32bc72138b0593be1e14c0f0087ec2cc41b61f80110402e8ff7f34748\", \"attribute_source\": \"infores:text-mining-provider-targeted\", \"attributes\": [{\"attribute_type_id\": \"biolink:supporting_text\", \"value\": \"It should also be noted that AA PCa was associated with a large number of up-regulated oncogenes (such as ITGA5, PIK3CB, PIK3AP, ITPR2, STAT1, CSNK2A1, MKK4, 14-3-3\\u03b5, MTOR and MCL1) as well as dysregulated unpaired miRNAs that are unique to AA PCa (e.g.\", \"value_type_id\": \"EDAM:data_3671\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:publications\", \"value\": \"PMC:4631799\", \"value_type_id\": \"biolink:Uriorcurie\", \"value_url\": \"https://pubmed.ncbi.nlm.nih.gov/PMC4631799/\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:supporting_text_located_in\", \"value\": \"DISCUSS\", \"value_type_id\": \"IAO_0000314\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:extraction_confidence_score\", \"value\": 0.9163411100614833, \"value_type_id\": \"EDAM:data_1772\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:subject_location_in_text\", \"value\": \"32|35\", \"value_type_id\": \"SIO:001056\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:object_location_in_text\", \"value\": \"143|150\", \"value_type_id\": \"SIO:001056\", \"attribute_source\": \"infores:text-mining-provider-targeted \"}, {\"attribute_type_id\": \"biolink:supporting_document_year\", \"value\": 2015, \"value_type_id\": \"UO:0000036\", \"attribute_source\": \"infores:pubmed\"}]}]")
   ("object" "UniProtKB:P68400")
   ("object_aspect_qualifier" "activity_or_abundance")
   ("object_direction_qualifier" "increased")
   ("predicate" "biolink:affects")
   ("qualified_predicate" "biolink:causes")
   ("score" "0.9163411100614833")
   ("subject" "DRUGBANK:DB03088")
   ("supporting_publications" "PMC:4631799")
   ("supporting_study_results"
    "tmkp:480d75d32bc72138b0593be1e14c0f0087ec2cc41b61f80110402e8ff7f34748")))
|#
   
