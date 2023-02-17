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

;; Can we now answer the old Alz query?

;; Which proteins with SH3 domains interact with tau protein in humans?

;; tau protein has CURI UniProtKB:P10636

;; find tau protein synonyms:
(define tau-protein-synonyms
  (curies->synonyms-in-db
   (list "UniProtKB:P10636")))

tau-protein-synonyms
;;=>
'("UMLS:C1446659"
  "REACT:R-HSA-201574"
  "REACT:R-HSA-9619519"
  "ENSEMBL:ENSG00000186868"
  "NCBIGene:4137"
  "REACT:R-HSA-350628"
  "HGNC:6893"
  "PR:P10636"
  "OMIM:157140"
  "REACT:R-HSA-9619521"
  "UniProtKB:P10636"
  "REACT:R-HSA-350642")

(length tau-protein-synonyms)
;; =>
12


;; We can get more CURIs by considering descendents of the synonyms:
(define tau-protein-synonyms-and-descendents
  (set->list
   (get-descendent-curies*-in-db
    (curies->synonyms-in-db
     (list "UniProtKB:P10636")))))

(length tau-protein-synonyms-and-descendents)
;;=>
141

;; These synonyms include lots of Protein Ontology (PR) entries (not sure if all are human).
;; Not sure if we want these descendent CURIs.

;; We will want to see which proteins interact with tau proteins.
;; The correct Biolink predicate seems to be:
;; "biolink:interacts_with"

;; We need to find proteins with SH3 domains.

;; Let's find the concept(s) of 'SH3 domain', without including
;; proteins with 'SH3' in the protein name.  We want proteins that
;; include SH3 domains, rather than proteins with SH3 in their name
;; (of course, there may be overlap!).

;; Protein category:
;; "biolink:Protein"

;; Might also want to include gene.

;; The mixin biolink:GeneOrGeneProduct seems most appropriate.
;; Make sure that we handle biolink:GeneOrGeneProduct correctly.

(get-non-deprecated-mixed-ins-and-descendent-classes*-in-db '("biolink:GeneOrGeneProduct"))
;; =>
(set
 "biolink:ProteinIsoform"
 "biolink:GeneProductIsoformMixin"
 "biolink:GeneOrGeneProduct"
 "biolink:Protein"
 "biolink:GeneProductMixin"
 "biolink:Gene")

#|
protein with SH3 domains
InterPro IPR001452 (SH3 domain)
http://www.ebi.ac.uk/interpro/entry/InterPro/IPR001452/?species=9606
Pfam PF00018 (SH3 domain)
Do we have InterPro or Pfam info?  Neither "Pfam:PF00018" nor "InterPro:IPR001452" have any synonyms in the current KGs.  Do I have the right CURIs?
|#

;; Try the name resolver SRI service
;; https://name-resolution-sri.renci.org/docs#/lookup/lookup_curies_lookup_post
;; Lookup: "SH3 domain"

;; Get back:
#|
"NCIT:C13340" ("SH3 Domain","SRC Homology Region 3 Domain")
"UMLS:C0282535" ("SH3 Domain","SH3 Domains","SH Z 003 DOMAIN","SRC Homology Region 3 Domain")
"NCIT:C14109" ("SH3-Binding Domain","SH3-Binding Motif")
"GO:0017124" ("SH3 domain binding")
"UMLS:C1519141" ("SH3-Binding Domain","SH3-Binding Motif")
"UMLS:C1149349" ("SH3 domain binding"),
"PANTHER.FAMILY:PTHR15706" ("SH3 MULTIPLE DOMAIN")
|#

(set->list
 (get-descendent-curies*-in-db
  (curies->synonyms-in-db
   (list "NCIT:C13340"
         "UMLS:C0282535"
         "NCIT:C14109"
         "GO:0017124"
         "UMLS:C1519141"
         "UMLS:C1149349"
         "PANTHER.FAMILY:PTHR15706"))))
;;=>
'("UMLS:C1519141"
  "NCIT:C14109"
  "NCIT:C14110"
  "UMLS:C1149349"
  "UMLS:C0282535"
  "GO:0017124"
  "NCIT:C13340"
  "PANTHER.FAMILY:PTHR15706")

;; Need to find proteins with the SH3 domain that occur in humans, that are known to interact with tau protein

;; "biolink:related_to" and subpredicates


(define related-to-SH3-domain-1
  (query:X->Known
   #f
   (set->list
    (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
     '("biolink:related_to")))
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db
      (list "NCIT:C13340"
            "UMLS:C0282535"
            "NCIT:C14109"
            "GO:0017124"
            "UMLS:C1519141"
            "UMLS:C1149349"
            "PANTHER.FAMILY:PTHR15706"))))))

(define related-to-SH3-domain-2
  (query:Known->X
   (set->list
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db
      (list "NCIT:C13340"
            "UMLS:C0282535"
            "NCIT:C14109"
            "GO:0017124"
            "UMLS:C1519141"
            "UMLS:C1149349"
            "PANTHER.FAMILY:PTHR15706"))))
   (set->list
    (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
     '("biolink:related_to")))
   #f))

(list->set (map (lambda (e) (match e
                              [`(,a ,b ,c ,d ,e . ,rest)
                               c]))
                related-to-SH3-domain-1))
;;=>
(set
 "biolink:disrupts"
 "biolink:related_to"
 "biolink:subclass_of"
 "biolink:coexists_with"
 "biolink:has_part"
 "biolink:regulates"
 "biolink:close_match"
 "biolink:affects"
 "biolink:enables")

;; Looks like "biolink:has_part" is the one we want.

(list->set (map (lambda (e) (match e
                              [`(,a ,b ,c ,d ,e . ,rest)
                               c]))
                related-to-SH3-domain-2))
;;=>
(set
 "biolink:has_part"
 "biolink:occurs_in"
 "biolink:subclass_of"
 "biolink:actively_involved_in"
 "biolink:interacts_with"
 "biolink:located_in"
 "biolink:regulates"
 "biolink:close_match"
 "biolink:affects"
 "biolink:enables")


(filter (lambda (e) e)
        (map (lambda (e) (match e
                   [`(,a ,b ,c ,d ,e . ,rest)
                    (if (equal? c "biolink:has_part")
                        (list a b c e)
                        #f)]))
     related-to-SH3-domain-1))
;;=>
#|
(("NCIT:C20973" "Amphiphysin" "biolink:has_part" "SH3 Domain")
 ("NCIT:C28720"
  "MYC Box-Dependent-Interacting Protein 1"
  "biolink:has_part"
  "SH3 Domain")
 ("NCIT:C30130" "Cdc42-Interacting Protein 4" "biolink:has_part" "SH3 Domain")
 ...)
|#

(curies->synonyms-in-db
 (list "NCIT:C28720"))
;;=>
'("NCIT:C28720")

;; Hmm.  Are these NCI Thesaurus CURIs the same as UMLS?  No!!

;; NCIT browser
;; https://ncit.nci.nih.gov/ncitbrowser/pages/home.jsf?version=23.01e
;; https://ncit.nci.nih.gov/ncitbrowser/pages/multiple_search.jsf?nav_type=terminologies

;; According to the NCIT browser,
"NCIT:C28720"
;; which is 'MYC Box-Dependent-Interacting Protein 1',
;; has synonyms:
"OMIM:601248"
"UniProtKB:O00499"
"UMLS:C0529765"

(curies->synonyms-in-db
 (list "OMIM:601248"))
;; =>
'("OMIM:601248"
  "ENSEMBL:ENSG00000136717"
  "ENSEMBL:LRG_873"
  "UMLS:C4316932"
  "HGNC:1052"
  "PR:O00499"
  "UniProtKB:O00499"
  "UMLS:C1332412"
  "NCBIGene:274")

(curies->synonyms-in-db
 (list "UniProtKB:O00499"))
;; =>
'("OMIM:601248"
  "ENSEMBL:ENSG00000136717"
  "UMLS:C4316932"
  "REACT:R-HSA-8868265"
  "HGNC:1052"
  "PR:O00499"
  "UniProtKB:O00499"
  "UMLS:C1332412"
  "NCBIGene:274")

(curies->synonyms-in-db
 (list "UMLS:C0529765"))
;; =>
'("UMLS:C0529765")

;; Sigh

;; We don't get back the NCIT mapping, alas
(curies->synonyms-in-db
 (list "OMIM:601248"
       "UniProtKB:O00499"
       "UMLS:C0529765"))
;; =>
'("UMLS:C0529765"
  "UniProtKB:O00499"
  "OMIM:601248"
  "ENSEMBL:ENSG00000136717"
  "ENSEMBL:LRG_873"
  "UMLS:C4316932"
  "REACT:R-HSA-8868265"
  "HGNC:1052"
  "PR:O00499"
  "UMLS:C1332412"
  "NCBIGene:274")

;; If we leave out "UMLS:C0529765"
;; at least we get a couple of UMLSs, but not
;; "UMLS:C0529765" as specified by NCIT.
(set->list
 (get-descendent-curies*-in-db
  (curies->synonyms-in-db
   (list "OMIM:601248"
         "UniProtKB:O00499"))))
;; =>
'("PR:000046483"
  "UniProtKB:O00499"
  "UniProtKB:O00499-6"
  "UniProtKB:O00499-9"
  "UMLS:C1706887"
  "PR:O00499-5"
  "PR:O00499-4"
  "PR:O00499-7"
  "PR:O00499-6"
  "PR:O00499-1"
  "PR:O00499-3"
  "PR:O00499-2"
  "PR:O00499-9"
  "PR:O00499-8"
  "UniProtKB:O00499-7"
  "UniProtKB:O00499-8"
  "ENSEMBL:ENSG00000136717"
  "ENSEMBL:LRG_873"
  "UniProtKB:O00499-3"
  "UMLS:C4316932"
  "UniProtKB:O00499-11"
  "NCBIGene:274"
  "UniProtKB:O00499-10"
  "UniProtKB:O00499-4"
  "UniProtKB:O00499-5"
  "PR:O00499-11"
  "UniProtKB:O00499-1"
  "PR:O00499-10"
  "OMIM:601248"
  "UMLS:C1332412"
  "REACT:R-HSA-8868265"
  "HGNC:1052"
  "UniProtKB:O00499-2"
  "PR:O00499")

(set->list
 (get-descendent-curies*-in-db
  (curies->synonyms-in-db
   (list "OMIM:601248"
         "UniProtKB:O00499"
         "UMLS:C0529765"))))
;; =>
'("PR:000046483"
  "UniProtKB:O00499"
  "UniProtKB:O00499-6"
  "UMLS:C0529765"
  "UniProtKB:O00499-9"
  "UMLS:C1706887"
  "PR:O00499-5"
  "PR:O00499-4"
  "PR:O00499-7"
  "PR:O00499-6"
  "PR:O00499-1"
  "PR:O00499-3"
  "PR:O00499-2"
  "PR:O00499-9"
  "PR:O00499-8"
  "UniProtKB:O00499-7"
  "UniProtKB:O00499-8"
  "ENSEMBL:ENSG00000136717"
  "ENSEMBL:LRG_873"
  "UniProtKB:O00499-3"
  "UMLS:C4316932"
  "UniProtKB:O00499-11"
  "NCBIGene:274"
  "UniProtKB:O00499-10"
  "UniProtKB:O00499-4"
  "UniProtKB:O00499-5"
  "PR:O00499-11"
  "UniProtKB:O00499-1"
  "PR:O00499-10"
  "OMIM:601248"
  "UMLS:C1332412"
  "REACT:R-HSA-8868265"
  "HGNC:1052"
  "UniProtKB:O00499-2"
  "PR:O00499")


(curies->synonyms-in-db
 (map car
      (filter (lambda (e) e)
              (map (lambda (e) (match e
                                 [`(,a ,b ,c ,d ,e . ,rest)
                                  (if (equal? c "biolink:has_part")
                                      (list a b c e)
                                      #f)]))
                   related-to-SH3-domain-1))))

(curies->synonyms-in-db
 (map (lambda (NCIT-CURI)
        (string-replace NCIT-CURI "NCIT:" "UMLS:"))
      (map car
           (filter (lambda (e) e)
                   (map (lambda (e) (match e
                                      [`(,a ,b ,c ,d ,e . ,rest)
                                       (if (equal? c "biolink:has_part")
                                           (list a b c e)
                                           #f)]))
                        related-to-SH3-domain-1)))))
;;=>
'()

;; Sigh

;; TODO
;; Try NCIT CURIs in th online node normalizer
;;
;; https://nodenormalization-sri.renci.org/docs#/default/get_normalized_node_handler_get_normalized_nodes_get
;;
;; Seems like the node normalizer has NCIT equivalents for diseases (such as NCIT:C34373), but not for genes/proteins (such as NCIT:C20973, NCIT:C28720, NCIT:C30130).
;;
;; Ask Chris B. about this
;;
;; Also ask the SRI KG people if they have NCIT and UMLS mappings.
;; See, for example,
;; https://obofoundry.org/ontology/ncit
;;
;; In the worst case, can create our own KG with the NCIT mappings.  Annoying...

;; TODO
;; Ask for an up-to-date version of the SRI KG

;; TODO
;;
;; find all human proteins that are known to interact with tau protein,
;; then take set intersection of those proteins and the proteins with
;; SH3 domains
;;
;; Is there a more direct way to express this query?
;;
;; The fact that we can't connect the NCIT CURIs to other CURIs in the
;; KGs is a bummer...
