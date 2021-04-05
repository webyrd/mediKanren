#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*****Application of mediKanren in case review*****;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Here I'm demonstrating how I used mediKanren for the PMI-20-10 case.
;; Date: 08/19/2020
;; For the case presentation, please check the file PMI-20-10.pdf

;; Participant is a 18 yo male who was diagnosed with Ulcerative Colitis at 3 yo. For the next 10 years, he was in
;; remission but his colitis flared again when he was 13.
;; For this case, there are many molecular data available such as Whole Exome Sequencing data (trio), microarray
;; gene expression data and in-vitro macrophage stimulation assay. Subject Matter Experts (SME) in molecular biology
;; , immunology and computational biology have looked at this case in details and hypothesized the mechanism
;; of disease: participant's IL1R1 variant may lead to aberrant IL1R signaling in T cells and result in his colitis
;; phenotype. Therefore, in this case, we used mediKanren to find drugs that target IL1R and IL1R signaling pathway.


;; First, load all data sources into RAM
(require "../../../medikanren/pieces-parts/query.rkt" "../pieces-parts/Thi-useful-functions.rkt")
                       
;; Find IL1R1 gene CURIE by looking up the identifiers in the GeneCards database
(define IL-1R1 "HGNC:5993")

;; Check if this CURIE has been synonymized in our Knowledge Graphs (KGs)
(curie-synonyms "HGNC:5993")

;; define the deccreases predicates to be used in run/graph drug->gene queries
(define decreases '("prevents"
                    "negatively_regulates"
                    "negatively_regulates__entity_to_entity"
                    "directly_negatively_regulates"
                    "biolink:negatively_regulates"
                    "biolink:negatively_regulates_entity_to_entity"
                    "biolink:directly_negatively_regulates"
                    "decreases_secretion_of"
                    "decreases_transport_of"
                    "decreases_activity_of"
                    "decreases_synthesis_of"
                    "decreases_expression_of"
                    "increases_degradation_of"
                    "disrupts"
                    "biolink:disease_disrupts"
                    "inhibits"
                    "inhibitor"
                    "antisense_inhibitor"
                    "gating_inhibitor"
                    "channel_blocker"
                    "treats"))
;; define drug
(define drug '(;; semmed
              "chemical_substance"
              ;; robokop
              "(\"named_thing\" \"chemical_substance\")"
              ;; orange?
              ;; rtx2
              "http://w3id.org/biolink/vocab/Drug"
              "http://w3id.org/biolink/vocab/ChemicalSubstance"
              "http://w3id.org/biolink/vocab/Chemical_Substance"
              ))

;; construct a query/graph to find drugs that targets/inhibits IL-1R1

(define q-IL-1R1 (query/graph ;; query/graph is an interface that alows us to query our data
                  ((S drug);; define the subject (S) to be drugs
                   (C IL-1R1));; define the object (C) to be IL1R1
                  ((S->C decreases));; define the verb/relationship between the subject and object
                  (S S->C C)));; define the pathway (i.e. direction of the story).
                              ;; This is where the subject is distinguished from the objects.


#|
***Please note that query/graph performs concept-synonymization/normalization under the hood.

Concept normalization allows traversal of different knowledge graphs and databases without
knowing all the equivalent CURIEs from different knowledge sources. This allows expansion
of one single CURIE which belongs to a single database to multiple equivalent CURIEs from
other databases, from where there may be productive search results for finding drugs.
|#

;; check the results of the query/graph output:
(report/query q-IL-1R1)

#| Output:

'((concepts: (S 10) (C 1)) (edges: (S->C 16)))

 At a glance, there are 10 drugs that inhibit IL1R1 and there are 16 different stories.
 Edges not only contain subject and object information, they also contain the details about the story, especially
 information such as provenance via Pubmed IDs.

|#

;; extract the drugs:
(define q-IL-1R1-drugs (curies/query q-IL-1R1 'S))

#|
Output:

'("CHEBI:15367"
  "CHEBI:28918"
  "CUI:C0002658"
  "CUI:C0012854"
  "CUI:C0022614"
  "CUI:C0035339"
  "CUI:C0041385"
  "CUI:C0084183"
  "CUI:C0376202"
  "CUI:C0684163")
|#

;; get the drug names from the drug curies
(map curie-synonyms/names q-IL-1R1-drugs)

#|
If we just want to map a CURIE to a name, we need to work with the list output:

(map car (map curie-synonyms/names q-IL-1R1-drugs))

Output:

'(("UMLS:C0022265" . "Isotretinoin")
  ("PUBCHEM:5816" . "Epinephrine")
  ("RXNORM:725" . "Amphetamine")
  ("UMLS:C0012854" . "DNA")
  ("MTHSPL:690G0D6V8H" . "Ketamine")
  ("NDFRT:N0000007700" . "Retinoids")
  ("CUI:C0041385" . "Tunicamycin")
  ("CUI:C0084183" . "prolinedithiocarbamate")
  ("CUI:C0376202" . "Sepranolone")
  ("UMLS:C0684163" . "membrane-bound receptors"))
|#

;; We can also take a look at the edges, which contains a lot more information about the relationship between the subject and the object
(define q-IL-1R1-edges (edges/query q-IL-1R1 'S->C))

;; get pubmed-ids from the eges:
(map pubmed-ids-from-edge q-IL-1R1-edges)

#|
Output:

'(("23351058")
  ("19746721")
  ("9515014")
  ("7780141")
  ("16586090")
  ("16586090")
  ("23351058")
  ("2341487")
  ("7780141")
  ("9515014")
  ("15176475")
  ("19746721")
  ("7780141")
  ("2341487")
  ("23351058")
  ("31191315"))
|#


#|
  The next strategy to solve this case is to target the IL-1R signaling pathway.

ILR-1 signaling pathway is a concept available in the GO database: (define IL-1R1-signaling "GO:0070498")

It would have been nice to be able to write a simple query to find drugs that target IL-1R signaling pathway.

***However, currently our KGs don't have a direct connection from GO to a drug database, so a mediKanren
query for drug that target a pathway would yield no results.

Here I'm manually define a pathway as a list of proteins belonging to that pathway.
|#

(define myD88 "HGNC:7562")
(define TAK1 "HGNC:6859")
(define NEMO "HGNC:5961")
(define IRAK1 "HGNC:6112")
(define IRAK2 "HGNC:6113")
(define TRAF6 "HGNC:12036")
(define MEK1 "HGNC:6848")
(define MEK3 "HGNC:6843")
(define MEK6 "HGNC:6846")
(define NFkB "HGNC:7794")
(define JNK "HGNC:6861")
(define p38 "HGNC:6876")
(define c-JNK "HGNC:6881")

(define IL1R-pathway (list myD88 TAK1 NEMO IRAK1 IRAK2 TRAF6 MEK1 MEK3 MEK6 NFkB JNK p38 c-JNK))

#|
Next I'm using a pre-defined find-drugs-for-genes function from Thi-useful-functions. This function, built upon a simple query/graph
such as the one above, will iterates through the list of subjects (i.e. genes to target in the IL1R-pathway).

;; Function description:

find-drugs-for-genes takes a gene-list and a list of drug-gene predicates (direction that we
want the drugs to influence on the genes, one gene at a time) and returns a list of two hash-tables: a sorted drug=>gene table and
a sorted gene=>drug table, which is sorted by the number of genes that have drugs available to modify them or
the number of drugs that can are available to target multiple genes.

|#
(define target-IL1R-pathway-results (find-drugs-for-genes IL1R-pathway negatively-regulates))

;; extract drug=>gene hash-table:
(define drug=>gene-IL1R-pathway (car target-IL1R-pathway-results))

#|
The output is a hash table with drug as key and genes as values.

This hashtable is sorted by how many genes the particular drug/chemical target.

'((("CHEBI:3962" . "curcumin")
   ("HGNC:6881" . "mitogen-activated protein kinase 8")
   ("HGNC:6876" . "mitogen-activated protein kinase 14")
   ("HGNC:7794" . "nuclear factor kappa B subunit 1")
   ("HGNC:6846" . "mitogen-activated protein kinase kinase 6")
   ("HGNC:6848" . "mitogen-activated protein kinase kinase kinase 1")
   ("HGNC:6859" . "mitogen-activated protein kinase kinase kinase 7")
   ("HGNC:7562" . "MYD88 innate immune signal transduction adaptor"))
  (("CHEBI:16243" . "quercetin")
   ("HGNC:6881" . "mitogen-activated protein kinase 8")
   ("HGNC:6876" . "mitogen-activated protein kinase 14")
   ("HGNC:7794" . "nuclear factor kappa B subunit 1")
   ("HGNC:12036" . "TNF receptor associated factor 6")
   ("HGNC:6859" . "mitogen-activated protein kinase kinase kinase 7")
   ("HGNC:7562" . "MYD88 innate immune signal transduction adaptor"))
  (("CHEBI:39867" . "valproic acid")
   ("HGNC:6881" . "mitogen-activated protein kinase 8")
   ("HGNC:7794" . "nuclear factor kappa B subunit 1")
   ("HGNC:6846" . "mitogen-activated protein kinase kinase 6")
   ("HGNC:6112" . "interleukin 1 receptor associated kinase 1")
   ("HGNC:7562" . "MYD88 innate immune signal transduction adaptor"))
[...]
|#

;; To summarize the results, we count the length of the table and the values for each drug-key

(length (map car drug=>gene-IL1R-pathway))

;; The results show that there are 444 drugs that target more than one members of the IL1R signaling pathway,
;; with the top 15 drugs target more than 5 members in this pathway. For example, curcumin targets 7 members (JNK,
;; p38, NFkB, MEK6, MEK1, TRAF6, myD88), quercetin targets 6 (JNK, p38, NFkB, TRAF6, MEK3, myD88)
;; members of the pathway.


;; extract gene=>drug hash-table:
(define gene=>drug-IL1R-pathway (cadr target-IL1R-pathway-results))
;; Similarly, the output of this gene=>drug table is a also hash table with gene as the key and drug as the value.
;; This table is also sorted by how many drugs available to target that gene.


























