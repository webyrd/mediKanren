#lang racket/base
(provide trapi-response)
(require
 "../../medikanren2/common.rkt"
  "../../medikanren2/lw-reasoning.rkt"
  racket/file racket/function racket/list racket/hash
  (except-in racket/match ==)
  racket/port
  racket/pretty
  racket/runtime-path
  racket/string
  json
  )

(define diabetes "DOID:9351")
(define diabetes2 "DOID:9352")

;; Reminder on 'semantic web'-style relations, defined in `medikanren2/common.rkt`

;; `is-a` is a short-hand for the "category" property
(run* c (is-a diabetes c))
(run* c (cprop diabetes "category" c))

;; and `triple` abstracts away the edge-id on triple relations
(run 1 (s p o) (triple s p o))
(run 1 (s p o) 
  (fresh (eid)
    (edge eid s o)
    (eprop eid "predicate" p)))

;; Query subclasses in the Biolink Ontology directly using `subclass-of`, `subclass-of*`, `subclass-of+`
(run* c (subclass-of+ c "biolink:regulates"))

;; Subclass relations on concept category
(run* c (is-a diabetes c))
(run* (c c^) (is-a/subclass* diabetes c c^))

;; Look for drugs that ameliorate (or a subclass thereof) diabetes
(run* (drug p) (triple/subclass drug "biolink:ameliorates" p diabetes))

;; ... or only subclasses thereof (mostly for testing)
(run* (drug p) (triple/subclass+ drug "biolink:ameliorates" p diabetes))

;; ... and drugs that ameliorate (or a subclass thereof) diabetes (or a subclass thereof)
(run* (drug p^ diabetes^) 
  (triple/subclass drug "biolink:ameliorates" p^ diabetes^)
  (subclass-of* diabetes^ diabetes))

;; ... there is a single relation that does reasoning on all three elements
;; but it can take a long time with run*
(run 1 (drug drug^ p diabetes^) 
  (triple/reasoning drug drug^ "biolink:ameliorates" p diabetes diabetes^))

(define gene "NCBIGene:7422")

(run* o (triple gene "biolink:regulates" o))

(run 10 (o p) (triple/subclass+ gene "biolink:regulates" p o))

;; ;; From working with Thi
;; (run* (graph drug d name) (quad graph drug "biolink:treats" d) (subclass-of+ d diabetes) (cprop drug "name" name))
;; (run 1 (d name dc dcname) (is-a d "biolink:Drug") (subclass-of d dc) (cprop d "name" name) (cprop dc "name" dcname))
;; (run 10 (d name dc dcname) (is-a d "biolink:Drug") (subclass-of d dc) (cprop d "name" name) (cprop dc "name" dcname))
;; (run 10 (d name dc dcc dcname) (is-a d "biolink:Drug") (subclass-of d dc) (cprop d "name" name) (cprop dc "name" dcname) (is-a dc dcc))
;; (run 10 (drug name dc dcc dcname) (is-a drug "biolink:Drug") (subclass-of drug dc) (cprop drug "name" name) (cprop dc "name" dcname) (is-a dc dcc))
;; (run 10 (drug name dc dcc dcname) (is-a drug "biolink:Drug") (subclass-of drug dc) (cprop drug "name" name) (cprop dc "name" dcname) (is-a dc dcc))
;; (run* (k v) (cprop    "CHEBI:65259" k v))
;; (run* (drug name) (subclass-of+     "CHEBI:65259" drug) (cprop drug "name" name))
;; "../../medikanren2/lw-reasoning.rkt"> (run* (drug name) (subclass-of+ drug    "CHEBI:51374") (cprop drug "name" name))
;; (run* (drug name) (triple drug "biolink:has_attribute" "CHEBI:65259") (cprop drug "name" name))
;; (run* (drug name) (triple drug "biolink:subclass_of" "CHEBI:65259") (cprop drug "name" name))



(define gene-to-disease-preds '("biolink:affects" ;rtx2_2021_02_04
                                "biolink:biomarker_for" ;rtx2_2021_02_04
                                "biolink:causes" ;rtx2_2021_02_04
                                "biolink:contributes_to" ;rtx2_2021_02_04
                                "biolink:correlated_with" ;rtx2_2021_02_04
                                "biolink:disrupts" ;rtx2_2021_02_04
                                "biolink:gene_associated_with_condition" ;rtx2_2021_02_04
                                "biolink:has_phenotype" ;rtx2_2021_02_04
                                "biolink:manifestation_of" ;rtx2_2021_02_04
                                "biolink:participates_in" ;rtx2_2021_02_04
                                "biolink:predisposes" ;rtx2_2021_02_04
                                "biolink:prevents" ;rtx2_2021_02_04
                                "biolink:treats" ;rtx2_2021_02_04
                                "biolink:contributes_to_morphology_of" ;rtx2_2021_02_0
                                ))

(define gene-or-protein '("biolink:Gene"
                          "biolink:GeneFamily"
                          "biolink:GeneProduct"
                          "biolink:GenomicEntity"
                          "biolink:MacromolecularComplex"
                          "biolink:MolecularEntity"
                          "biolink:Protein")) 



;; synonyms

(define anxiety "HP:0000739")
(define apnea "HP:0002104")
(define epilepsy "MONDO:0005027")
(define vomiting "HP:0002013")
(define spasticity "HP:0001257")
(define ID "HP:0001249")
(define gas-reflux "HP:0002020")
(define autism "MONDO:0005260")

; some of the phenotypes are not actionable (no drugs)
;(define feeding-diff "HP:0011968") ;; no drugs for this phenotype
;(define flat-nose "HP:0000457")
;(define loss-speech "HP:0002371")
;(define low-set-ears "HP:0000369")
;(define microcephaly "HP:0000252")
;(define small-stature "HP:0004322")
;(define small-teeth "HP:0000691")
;(define speech-delay "HP:0000750")
;(define global-dev-delay "HP:0001263")
;(define episodic-vomit "HP:0002572")
;(define nystagmus "HP:0000639")
;(define strabismus "HP:0000486")

(define anxiety "HP:0000739")
(define anxieties (cons anxiety (run* a (direct-synonym anxiety a))))
;; human-readable
;; (run* (a name) 
;;   (fresh (syn)
;;     (membero syn anxieties)
;;     (subclass-of+ a syn)
;;     (cprop a "name" name))))
(define sub-anxieties
  (remove-duplicates
   (run* a 
     (fresh (syn) 
       (membero syn anxieties)
       (subclass-of* a syn)))))
(define sub-anxieties-synonyms
  (remove-duplicates
   (run* a
     (fresh (anx)
       (membero anx sub-anxieties)
       (conde ((direct-synonym anx a))
              ((direct-synonym a anx)))))))
(define phens sub-anxieties-synonyms)
;; why so long? (take out constraints, runs faster)
(time (run 10 (gene name p phenotype pname) 
     (fresh (cat) 
       (triple gene p phenotype)
       (membero cat gene-or-protein)
       (membero phenotype phens)
       (membero p gene-to-disease-preds)
       (is-a gene cat)
       (cprop gene "name" name)
       (cprop phenotype "name" pname)
       )))


;; performance puzzle

(time (length (run 100 (gene p phenotype ) 
         (triple gene p phenotype)
       (membero phenotype phens)
)))

(time (length (run 100 (gene p phenotype ) 
         (triple gene p phenotype)
       (membero phenotype phens)
       (membero p gene-to-disease-preds))))

(time (length (run 100 (gene p phenotype ) 
     (fresh (cat) 
       (triple gene p phenotype)
       (membero cat gene-or-protein)
       (membero phenotype phens)
       (membero p gene-to-disease-preds)
       ))))
