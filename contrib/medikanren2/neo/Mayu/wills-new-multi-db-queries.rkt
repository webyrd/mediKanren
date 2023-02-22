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

(define kxk
  (lambda (c1 c2)
    (query:Known->X->Known
     (set->list
      (get-descendent-curies*-in-db
       (curies->synonyms-in-db (list c1))))
     '("biolink:same_as")
     #f
     '("biolink:treats")
     (set->list
      (get-descendent-curies*-in-db
       (curies->synonyms-in-db (list c2)))))))

;; imatinib ->biolink:same_as-> X ->biolink:treats-> gastrointestinal stromal tumor
(define a (kxk "UMLS:C0935989" "UMLS:C0238198"))

;; Example of a result for 'a' that spans KGs:

#|
("CHEBI:45783"
   "imatinib"
   "biolink:same_as"
   "DRUGBANK:DB00619"
   "Imatinib"
   "biolink:treats"
   "MONDO:0011719"
   "gastrointestinal stromal tumor"
   ("id"
    "CHEBI:45783---owl:sameAs---None---None---None---DRUGBANK:DB00619---UNICHEM_source:")
   ("knowledge_source" "infores:pathwhiz|infores:unichem")
   ("object" "DRUGBANK:DB00619")
   ("predicate" "biolink:same_as")
   ("publications_info" "{}")
   ("subject" "CHEBI:45783")
   ("_attributes"
    "[{\"attribute_type_id\": \"biolink:original_knowledge_source\", \"value\": \"infores:text-mining-provider-targeted\", \"value_type_id\": \"biolink:InformationResource\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:supporting_data_source\", \"value\": \"infores:pubmed\", \"value_type_id\": \"biolink:InformationResource\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:has_evidence_count\", \"value\": 6116, \"value_type_id\": \"biolink:EvidenceCount\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:tmkp_confidence_score\", \"value\": 0.9959426746860693, \"value_type_id\": \"biolink:ConfidenceLevel\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:supporting_document\", \"value\": \"PMC3636436|PMC5590134|PMC6786996|PMID:16204927|PMID:21387287\", \"value_type_id\": \"biolink:Publication\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:supporting_study_result\", \"value\": \"tmkp:65bcc14a34089bd25c3db4ddc71cf57164f4d38a05f36e012b32335d18223c50\", \"value_type_id\": \"biolink:TextMiningResult\", \"attribute_source\": \"infores:text-mining-provider-targeted\", \"attributes\": [{\"attribute_type_id\": \"biolink:supporting_text\", \"value\": \"Imatinib as adjuvant therapy for gastrointestinal stromal tumors: a systematic review\", \"value_type_id\": \"EDAM:data_3671\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:supporting_document\", \"value\": \"PMC3636436\", \"value_type_id\": \"biolink:Publication\", \"value_url\": \"https://pubmed.ncbi.nlm.nih.gov/PMC3636436/\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:supporting_text_located_in\", \"value\": \"REF\", \"value_type_id\": \"IAO_0000314\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:extraction_confidence_score\", \"value\": 0.9995431, \"value_type_id\": \"EDAM:data_1772\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:subject_location_in_text\", \"value\": \"0|8\", \"value_type_id\": \"SIO:001056\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:object_location_in_text\", \"value\": \"33|64\", \"value_type_id\": \"SIO:001056\", \"attribute_source\": \"infores:text-mining-provider-targeted \"}, {\"attribute_type_id\": \"biolink:supporting_document_year\", \"value\": 2012, \"value_type_id\": \"UO:0000036\", \"attribute_source\": \"infores:pubmed\"}]}, {\"attribute_type_id\": \"biolink:supporting_study_result\", \"value\": \"tmkp:bd0753f3583f2decb8013ae694183fa5dda87764b52ef92c930abd1d624236da\", \"value_type_id\": \"biolink:TextMiningResult\", \"attribute_source\": \"infores:text-mining-provider-targeted\", \"attributes\": [{\"attribute_type_id\": \"biolink:supporting_text\", \"value\": \"Imatinib as adjuvant therapy for gastrointestinal stromal tumors: a systematic review\", \"value_type_id\": \"EDAM:data_3671\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:supporting_document\", \"value\": \"PMC5590134\", \"value_type_id\": \"biolink:Publication\", \"value_url\": \"https://pubmed.ncbi.nlm.nih.gov/PMC5590134/\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:supporting_text_located_in\", \"value\": \"REF\", \"value_type_id\": \"IAO_0000314\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:extraction_confidence_score\", \"value\": 0.9995431, \"value_type_id\": \"EDAM:data_1772\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:subject_location_in_text\", \"value\": \"0|8\", \"value_type_id\": \"SIO:001056\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:object_location_in_text\", \"value\": \"33|64\", \"value_type_id\": \"SIO:001056\", \"attribute_source\": \"infores:text-mining-provider-targeted \"}, {\"attribute_type_id\": \"biolink:supporting_document_year\", \"value\": 2017, \"value_type_id\": \"UO:0000036\", \"attribute_source\": \"infores:pubmed\"}]}, {\"attribute_type_id\": \"biolink:supporting_study_result\", \"value\": \"tmkp:cf744da3e48412c6341b1a80e9b9fece28ff3a555d0a040eec8391e59d684427\", \"value_type_id\": \"biolink:TextMiningResult\", \"attribute_source\": \"infores:text-mining-provider-targeted\", \"attributes\": [{\"attribute_type_id\": \"biolink:supporting_text\", \"value\": \"Imatinib as adjuvant therapy for gastrointestinal stromal tumors: a systematic review\", \"value_type_id\": \"EDAM:data_3671\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:supporting_document\", \"value\": \"PMC6786996\", \"value_type_id\": \"biolink:Publication\", \"value_url\": \"https://pubmed.ncbi.nlm.nih.gov/PMC6786996/\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:supporting_text_located_in\", \"value\": \"REF\", \"value_type_id\": \"IAO_0000314\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:extraction_confidence_score\", \"value\": 0.9995431, \"value_type_id\": \"EDAM:data_1772\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:subject_location_in_text\", \"value\": \"0|8\", \"value_type_id\": \"SIO:001056\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:object_location_in_text\", \"value\": \"33|64\", \"value_type_id\": \"SIO:001056\", \"attribute_source\": \"infores:text-mining-provider-targeted \"}, {\"attribute_type_id\": \"biolink:supporting_document_year\", \"value\": 2019, \"value_type_id\": \"UO:0000036\", \"attribute_source\": \"infores:pubmed\"}]}, {\"attribute_type_id\": \"biolink:supporting_study_result\", \"value\": \"tmkp:921205bc03c6569adb4df892ee82cd938606be62661023435625b6f8de8a50b8\", \"value_type_id\": \"biolink:TextMiningResult\", \"attribute_source\": \"infores:text-mining-provider-targeted\", \"attributes\": [{\"attribute_type_id\": \"biolink:supporting_text\", \"value\": \"Imatinib mesylate as therapy for gastrointestinal stromal tumor.\", \"value_type_id\": \"EDAM:data_3671\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:supporting_document\", \"value\": \"PMID:16204927\", \"value_type_id\": \"biolink:Publication\", \"value_url\": \"https://pubmed.ncbi.nlm.nih.gov/16204927/\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:supporting_text_located_in\", \"value\": \"title\", \"value_type_id\": \"IAO_0000314\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:extraction_confidence_score\", \"value\": 0.99954236, \"value_type_id\": \"EDAM:data_1772\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:subject_location_in_text\", \"value\": \"0|8\", \"value_type_id\": \"SIO:001056\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:object_location_in_text\", \"value\": \"33|63\", \"value_type_id\": \"SIO:001056\", \"attribute_source\": \"infores:text-mining-provider-targeted \"}, {\"attribute_type_id\": \"biolink:supporting_document_year\", \"value\": 2005, \"value_type_id\": \"UO:0000036\", \"attribute_source\": \"infores:pubmed\"}]}, {\"attribute_type_id\": \"biolink:supporting_study_result\", \"value\": \"tmkp:11f292a42ef4384d1544ee6c7dd9b3d5c2fe803a7f71f26243eedee6443f6718\", \"value_type_id\": \"biolink:TextMiningResult\", \"attribute_source\": \"infores:text-mining-provider-targeted\", \"attributes\": [{\"attribute_type_id\": \"biolink:supporting_text\", \"value\": \"Imatinib as adjuvant therapy for gastrointestinal stromal tumors: a systematic review.\", \"value_type_id\": \"EDAM:data_3671\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:supporting_document\", \"value\": \"PMID:21387287\", \"value_type_id\": \"biolink:Publication\", \"value_url\": \"https://pubmed.ncbi.nlm.nih.gov/21387287/\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:supporting_text_located_in\", \"value\": \"title\", \"value_type_id\": \"IAO_0000314\", \"attribute_source\": \"infores:pubmed\"}, {\"attribute_type_id\": \"biolink:extraction_confidence_score\", \"value\": 0.9995378, \"value_type_id\": \"EDAM:data_1772\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:subject_location_in_text\", \"value\": \"0|8\", \"value_type_id\": \"SIO:001056\", \"attribute_source\": \"infores:text-mining-provider-targeted\"}, {\"attribute_type_id\": \"biolink:object_location_in_text\", \"value\": \"33|64\", \"value_type_id\": \"SIO:001056\", \"attribute_source\": \"infores:text-mining-provider-targeted \"}, {\"attribute_type_id\": \"biolink:supporting_document_year\", \"value\": 2011, \"value_type_id\": \"UO:0000036\", \"attribute_source\": \"infores:pubmed\"}]}]")
   ("association_type"
    "biolink:ChemicalToDiseaseOrPhenotypicFeatureAssociation")
   ("confidence_score" "0.9959426746860693")
   ("id" "cfcb7c55f657acae4e169566d0aab4d8e5e27ae861ee4d47aac5d30020afb539")
   ("object_curie" "MONDO:0011719")
   ("predicate" "biolink:treats")
   ("subject_curie" "DRUGBANK:DB00619")
   ("supporting_publication_identifiers"
    "PMC3636436|PMC5590134|PMC6786996|PMID:16204927|PMID:21387287")
   ("supporting_study_result_identifiers"
    "tmkp:65bcc14a34089bd25c3db4ddc71cf57164f4d38a05f36e012b32335d18223c50|tmkp:bd0753f3583f2decb8013ae694183fa5dda87764b52ef92c930abd1d624236da|tmkp:cf744da3e48412c6341b1a80e9b9fece28ff3a555d0a040eec8391e59d684427|tmkp:921205bc03c6569adb4df892ee82cd938606be62661023435625b6f8de8a50b8|tmkp:11f292a42ef4384d1544ee6c7dd9b3d5c2fe803a7f71f26243eedee6443f6718"))
|#


;; RTX KG2:
;; (query:X->Known #f '("biolink:treats") (list "DOID:9351"))

;; RTX KG2:
;; (query:X->Known #f '("biolink:subclass_of") (list "DOID:9351"))

;; Robokop:
;; (query:X->Known #f '("biolink:subclass_of") (list "NCBITaxon:1748027"))

;; Text Mining
;; (query:X->Known #f '("biolink:entity_negatively_regulates_entity") (list "UniProtKB:P47712"))

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

(length diabetes-treatments)
;; 20042


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

(length diabetes-causes)
;; =>
9297

(sort (remove-duplicates (map car diabetes-causes)) string<=?)

(filter (lambda (e) (string=? "water" (cadr e))) diabetes-causes)
