#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers.rkt"
 racket/match
 racket/set
 racket/pretty
 racket/string)

#|
What treats achromatopsia (color blindness)?

An associated gene:
CNGB3

ACHM3
|#

;; achromatopsia (DOID:13911)
(define all-achromatopsia-curies
  (get-descendent-curies*-in-db
   (curies->synonyms-in-db
    (list "DOID:13911"))))
;; http://www.informatics.jax.org/disease/DOID:13911
;; ICD10CM:H53.51, ICD9CM:368.54, MESH:D003117, NCI:C84528, ORDO:49382, UMLS_CUI:C0152200 
'("DOID:0050679"
  "UMLS:C0339537"
  "MONDO:0014677"
  "MESH:C536021"
  "ORPHANET:16"
  "DOID:13911"
  "MONDO:0010563"
  "ORPHANET:49382"
  "NCIT:C84528"
  "MONDO:0009003"
  "HP:0011516"
  "UMLS:C0152200"
  "MONDO:0012398"
  "DOID:0110009"
  "DOID:0110007"
  "DOID:0110008"
  "DOID:0110010"
  "MONDO:0013465"
  "MONDO:0009875"
  "UMLS:C0302129"
  "MONDO:0018852")

;; achromatopsia 3 (DOID:0110008)
(define all-achromatopsia3-curies
  (get-descendent-curies*-in-db
   (curies->synonyms-in-db
    (list "DOID:0110008"))))
;; http://www.informatics.jax.org/disease/DOID:0110008
;; ACHM1; ACHM3; Pingelapese blindness; RMCH1; rod monochromacy 1; rod monochromatism 1 
'("OMIM:262300"
 "OMIM:MTHU004637"
 "DOID:0110008"
 "MONDO:0009875"
 "OMIM:MTHU005722"
 "OMIM:MTHU028493"
 "OMIM:MTHU002321"
 "OMIM:MTHU023011"
 "OMIM:MTHU023012"
 "OMIM:MTHU023013"
 "OMIM:MTHU023014"
 "OMIM:MTHU036378"
 "UMLS:C1849792"
 "MESH:C536129")


;; What treats achromatopsia?
(define achromatopsia-treatments
  (query:X->Known
   ;; Looking for any type of treatment
   #f
   ;; list of predicates
   (set->list (get-predicate-descendents-in-db "biolink:treats"))
   ;; list of known CURIES (for achromatopsia)
   (set->list all-achromatopsia-curies)))
;; =>
'(("UMLS:C0009836"
   "Contact Lenses"
   "biolink:treats"
   "UMLS:C0302129"
   "Achromatopsia 1"
   ("id" "UMLS:C0009836---SEMMEDDB:treats---UMLS:C0302129---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0302129")
   ("predicate" "biolink:treats")
   ("publications" "PMID:315420")
   ("publications_info"
    "{\"PMID:315420\": {\"publication date\": \"1979 Jul\", \"sentence\": \"A young female rod monochromat is treated optometrically with binocular contact lenses to relieve light dazzlement and a monocular lens to improve hue discrimination.\", \"subject score\": 901, \"object score\": 836}}")
   ("subject" "UMLS:C0009836"))
  
  ("UMLS:C0012315"
   "Dihydropyridines"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C0012315---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications" "PMID:33827965")
   ("publications_info"
    "{\"PMID:33827965\": {\"publication date\": \"2021 Apr 07\", \"sentence\": \"Active compounds and our structure-activity correlated data for the dihydropyridine compound class may provide valuable information for developing a treatment of the trafficking defect in achromatopsia.\", \"subject score\": 851, \"object score\": 1000}}")
   ("subject" "UMLS:C0012315"))
  
  ("UMLS:C0220825"
   "Evaluation"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C0220825---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications" "PMID:32881472")
   ("publications_info"
    "{\"PMID:32881472\": {\"publication date\": \"2020 Aug 03\", \"sentence\": \"Methods: Affected members from three pedigrees with classical enhanced S-cone syndrome (ESCS; Pedigree 1), congenital stationary night blindness (CSNB; Pedigree 2), and achromatopsia (ACHM; Pedigree 3), respectively, underwent detailed ophthalmologic evaluation, optical coherence tomography, and electroretinography.\", \"subject score\": 851, \"object score\": 1000}}")
   ("subject" "UMLS:C0220825"))
  
  ("UMLS:C0699680"
   "Metric (substance)"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C0699680---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications" "PMID:25277229")
   ("publications_info"
    "{\"PMID:25277229\": {\"publication date\": \"2014 Oct 02\", \"sentence\": \"CONCLUSIONS: We present cone reflectivity as a metric that can be used to characterize cone structure in ACHM.\", \"subject score\": 1000, \"object score\": 1000}}")
   ("subject" "UMLS:C0699680"))
  
  ("UMLS:C1261322"
   "Evaluation procedure"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C1261322---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications" "PMID:25103266|PMID:28145975|PMID:30513534")
   ("publications_info"
    "{\"PMID:25103266\": {\"publication date\": \"2014 Aug 07\", \"sentence\": \"METHODS: Thirty-eight molecularly confirmed ACHM subjects underwent serial assessments, including spectral domain optical coherence tomography (SD-OCT), microperimetry, and fundus autofluorescence (FAF).\", \"subject score\": 872, \"object score\": 740}, \"PMID:28145975\": {\"publication date\": \"2017 Oct\", \"sentence\": \"REPEATABILITY AND LONGITUDINAL ASSESSMENT OF FOVEAL CONE STRUCTURE IN CNGB3-ASSOCIATED ACHROMATOPSIA.\", \"subject score\": 888, \"object score\": 827}, \"PMID:30513534\": {\"publication date\": \"2018 Dec 03\", \"sentence\": \"Longitudinal Assessment of Retinal Structure in Achromatopsia Patients With Long-Term Follow-up.\", \"subject score\": 888, \"object score\": 888}}")
   ("subject" "UMLS:C1261322"))
  
  ("UMLS:C0021102"
   "Implants"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C0021102---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications" "PMID:26567794")
   ("publications_info"
    "{\"PMID:26567794\": {\"publication date\": \"2015 Oct\", \"sentence\": \"However, testing CNTF-releasing implant in human CNGB3 achromats failed to show benefit.\", \"subject score\": 868, \"object score\": 749}}")
   ("subject" "UMLS:C0021102"))
  
  ("UMLS:C0009836"
   "Contact Lenses"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C0009836---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications" "PMID:17208670|PMID:17601566")
   ("publications_info"
    "{\"PMID:17601566\": {\"publication date\": \"2007 Jul\", \"sentence\": \"The use of tinted contact lenses in the management of achromatopsia.\", \"subject score\": 901, \"object score\": 1000}, \"PMID:17208670\": {\"publication date\": \"2007 Jan\", \"sentence\": \"The use of tinted contact lenses in the management of achromatopsia.\", \"subject score\": 901, \"object score\": 1000}}")
   ("subject" "UMLS:C0009836"))
  
  ("UMLS:C0087111"
   "Therapeutic procedure"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C0087111---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications" "PMID:29926749|PMID:30289319")
   ("publications_info"
    "{\"PMID:29926749\": {\"publication date\": \"2018 Jul 30\", \"sentence\": \"The purpose of this study was to evaluate long-term efficacy and safety results of treatment, findings that hold great relevance for clinical trials that started recently in CNGA3 achromatopsia patients.\", \"subject score\": 1000, \"object score\": 790}, \"PMID:30289319\": {\"publication date\": \"2018 Dec\", \"sentence\": \"CONCLUSIONS: These novel variants expand the genotypes associated with ACHM and may help in future therapy development for ACHM.\", \"subject score\": 851, \"object score\": 1000}}")
   ("subject" "UMLS:C0087111"))
  
  ("UMLS:C0011900"
   "Diagnosis"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C0011900---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications" "PMID:29257187")
   ("publications_info"
    "{\"PMID:29257187\": {\"publication date\": \"2018 Mar 01\", \"sentence\": \"The purpose of this review was to survey the current knowledge on diagnosis and treatment options in achromatopsia.\", \"subject score\": 1000, \"object score\": 1000}}")
   ("subject" "UMLS:C0011900"))
  
  ("UMLS:C1304648"
   "Gene replacement therapy"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C1304648---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications" "PMID:26407004")
   ("publications_info"
    "{\"PMID:26407004\": {\"publication date\": \"2015\", \"sentence\": \"These newly characterized large animal models not only provide a valuable system for studying cone-specific CNG channel function in health and disease, but also represent prime candidates for proof-of-concept studies of CNGA3 gene replacement therapy for ACHM patients.\", \"subject score\": 923, \"object score\": 888}}")
   ("subject" "UMLS:C1304648"))
  
  ("UMLS:C1533685"
   "Injection procedure"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C1533685---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications" "PMID:28478700")
   ("publications_info"
    "{\"PMID:28478700\": {\"publication date\": \"2017 06\", \"sentence\": \"Safety and Efficacy Evaluation of rAAV2tYF-PR1.7-hCNGA3 Vector Delivered by Subretinal Injection in CNGA3 Mutant Achromatopsia Sheep.\", \"subject score\": 861, \"object score\": 775}}")
   ("subject" "UMLS:C1533685"))
  
  ("UMLS:C1520007"
   "Viral Vector"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C1520007---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications" "PMID:26603570")
   ("publications_info"
    "{\"PMID:26603570\": {\"publication date\": \"2016 Jan\", \"sentence\": \"These results have informed the design of an AAV vector for treatment of patients with achromatopsia.\", \"subject score\": 861, \"object score\": 1000}}")
   ("subject" "UMLS:C1520007"))
  
  ("UMLS:C1511790"
   "Detection"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C1511790---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications" "PMID:32531858|PMID:3486271")
   ("publications_info"
    "{\"PMID:3486271\": {\"publication date\": \"1986 Feb\", \"sentence\": \"Threshold detection for sine-wave grating stimuli of varying spatial and temporal frequency was used to investigate the nature of spatial and temporal post-receptoral sensitivity in the typical, complete achromat.\", \"subject score\": 888, \"object score\": 740}, \"PMID:32531858\": {\"publication date\": \"2020 Jun 12\", \"sentence\": \"We achieved a molecular diagnostic rate of 35%-95%, depending on the clinical entities, with a high detection rate for achromatopsia, retinoschisis and choroideremia, and a low detection rate for central areolar choroidal dystrophy and macular dystrophy.\", \"subject score\": 851, \"object score\": 1000}}")
   ("subject" "UMLS:C1511790"))
  
  ("UMLS:C0017296"
   "gene therapy"
   "biolink:treats"
   "UMLS:C0152200"
   "Achromatopsia"
   ("id" "UMLS:C0017296---SEMMEDDB:treats---UMLS:C0152200---SEMMEDDB:")
   ("knowledge_source" "infores:semmeddb")
   ("object" "UMLS:C0152200")
   ("predicate" "biolink:treats")
   ("publications"
    "PMID:20378608|PMID:22882429|PMID:23141518|PMID:24658860|PMID:25616768|PMID:25855802|PMID:26087757|PMID:26196097|PMID:28095637|PMID:29259520|PMID:30324456|PMID:32352493|PMID:33528822|PMID:34006508")
   ("publications_info"
    "{\"PMID:24658860\": {\"publication date\": \"2014 Mar\", \"sentence\": \"The outstanding results derived from the animal model are the starting point for the first human translation of a gene therapy for achromatopsia in Germany.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:23141518\": {\"publication date\": \"2012 Aug\", \"sentence\": \"With the development of molecular genetics and the therapeutic gene replacement technology, the adeno-associated viral (AAV) vector-mediated gene therapy for achromatopsia in the preclinical animal experiments achieved encouraging progress in the past years.\", \"subject score\": 763, \"object score\": 1000}, \"PMID:22882429\": {\"publication date\": \"2012 Sep\", \"sentence\": \"Recent successes in using gene therapy to treat canine achromatopsia, X-linked progressive retinal atrophy (PRA) and the more severe rapid degenerations such as rod-cone dysplasia type 3 may lead also to the translation to human clinical trials.\", \"subject score\": 1000, \"object score\": 888}, \"PMID:20378608\": {\"publication date\": \"2010 Jul 01\", \"sentence\": \"Our results hold promise for future clinical trials of cone-directed gene therapy in achromatopsia and other cone-specific disorders.\", \"subject score\": 861, \"object score\": 1000}, \"PMID:29259520\": {\"publication date\": \"2017 12\", \"sentence\": \"The evidence to date suggests that gene therapy for achromatopsia will need to be applied early in childhood to be effective.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:25855802\": {\"publication date\": \"2015 Jul 01\", \"sentence\": \"Demonstration of gene therapy in a cone-dominant mouse model by IVit delivery provides a potential alternative vector delivery mode for safely transducing foveal cones in achromatopsia patients and in other human retinal diseases affecting foveal function.\", \"subject score\": 1000, \"object score\": 888}, \"PMID:26087757\": {\"publication date\": \"2015 Sep\", \"sentence\": \"The results support future application of subretinal AAV5-mediated gene-augmentation therapy in CNGA3 achromatopsia patients.\", \"subject score\": 803, \"object score\": 790}, \"PMID:28095637\": {\"publication date\": \"2017 Mar\", \"sentence\": \"Gene therapy for achromatopsia.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:30324456\": {\"publication date\": \"2019\", \"sentence\": \"One example is gene therapy for achromatopsia which affects daylight vision.\", \"subject score\": 812, \"object score\": 1000}, \"PMID:25616768\": {\"publication date\": \"2015 May\", \"sentence\": \"The present study took steps toward performing a trial of gene therapy in ACHM by characterizing the genetics of ACHM in Israel and the Palestinian Territories and analyzing retinal function and structure in CNGA3 ACHM patients from the Israeli-Palestinian population and US patients with other origins.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:26196097\": {\"publication date\": \"2015 Jul\", \"sentence\": \"The potential for the treatment of achromatopsia in humans with gene therapy shows great promise.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:32352493\": {\"publication date\": \"2020 Apr 30\", \"sentence\": \"Objective: To assess safety and vision outcomes of supplemental gene therapy with adeno-associated virus (AAV) encoding CNGA3 (AAV8.CNGA3) in patients with CNGA3-linked achromatopsia.\", \"subject score\": 901, \"object score\": 840}, \"PMID:33528822\": {\"publication date\": \"2021 Feb 02\", \"sentence\": \"RESULTS: Various adenovirus vectors have been deployed to test the efficacy of gene therapy for achromatopsia in animals and humans.\", \"subject score\": 1000, \"object score\": 1000}, \"PMID:34006508\": {\"publication date\": \"2021 May 18\", \"sentence\": \"AIMS: To determine long-term safety and efficacy outcomes of a subretinal gene therapy for CNGA3-associated achromatopsia.\", \"subject score\": 901, \"object score\": 802}}")
   ("subject" "UMLS:C0017296"))
  )
