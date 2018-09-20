;;; Workflow 1

#|
concept format (subject or object), without dbname at front:

`(,cid ,cui ,name (,catid . ,cat) . ,props)

concept format (subject or object), with dbname at front (as used in fuzzy-concepto):

`(,dbname ,cid ,cui ,name (,catid . ,cat) . ,props)


edge format, without dbname at front:

`(,eid (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
       (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
       (,pid . ,pred) . ,eprops)

edge format, with dbname at front (as used in edgeo):

`(,dbname ,eid (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
               (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
               (,pid . ,pred) . ,eprops)
|#


#|
NOTES

TH2LCRR in concept 1
has_phenotype aggressive behavior
("publications"
    .
    "['http://www.ncbi.nlm.nih.gov/pubmed/27611488', 'http://www.ncbi.nlm.nih.gov/pubmed/10220955', 'http://www.ncbi.nlm.nih.gov/pubmed/10557854', 'http://www.ncbi.nlm.nih.gov/pubmed/15611893', 'http://www.ncbi.nlm.nih.gov/pubmed/550177', 'http://www.ncbi.nlm.nih.gov/pubmed/14572559']")


*** talk with Greg and Chris about doing a clique merge to avoid
having duplicate concepts that are equivalent but with dfferent
sources


*** Ask Greg to take a look:
monarch-lite has many type of 'diabetes', but apparently none has 'has_phenotype' predicate

how to find monogenetic diabetes in these data sources?  I suppose, look for genes that cause diabetes?  how do we know it is truly monogenic?

one such gene is KCNJ11

rtx:
KCNJ11 has_phenotype HP:00005978 Type II diabetes mellitus (source: biolink)

rtx:
KCNJ11 causes_or_contributes_to OMIM:606176 DIABETES MELITUS, PERMANENT NEONATAL; PNDM (source: OMIM)

rtx:
KCNJ11 participates_in 'Defective ABCD1 causes ALD'   what??  does that make sense?  (reactome pathway)

rtx and monarch:
'HP:00005978 Type II diabetes mellitus' subclass_of 'HP:0000819 Diabetes mellitus'

'HP:0000819 Diabetes mellitus' subclass_of 'HP:0011014 Abnormal glucose homeostasis'

*** unable to reproduce in mediKanren GUI--run queries in Racket to see if any fields that should be strings are actually #f
monarch:
diabetes mellitus in concept 1 gives:
OMIA:000284-9825 with 'Name' #f   what gives?

also returns 'type 1 diabetes mellitus 7', 'type 1 diabetes mellitus 8', etc, as MONDO terms--what does this mean???


monarch:
X has_phenotype HPO:0000819 Diabetes mellitus

2729 results
appear to all be genes
but, dfferent categories:
HGNC (2 . [molec entity, gene, sequence feat])
HGNC (1 . [gene, sequence feature])
NCBIGene (1 . [gene, sequence feature])

interesting--have both for HGNC--what is the difference?  are the gene names duplicated?

between OMIM, semmed, HGNC, etc, tricky to try to find the equivalent gene name/protein across sources (UMLS term for semmed, UniProt for OMIM in rtx, HGNC in monarch)


Monarch 

|#



#|
		What are some potential treatments for [common condition], based on knowledge of related rare conditions?	
Instance		"Monogenic diabetes and sporadic (common) diabetes"

Component	Standard Query Type Id	Logical Operation	Bid
	Q61	Path/Module 1 (condition similarity)	
*** MOD1.1	Q5	What are the defining symptoms / phenotypes of [condition x]?	0/2/3 (what does "defining" mean?)
*** MOD1.2	Q60	What conditions present [symptoms]?	3
MOD1.3	Filter	Filter [conditions] to only keep ones with defined genetic causes.	0/2
MOD1.4	Filter	What subset of conditions are most representative of [conditions]? (find archetypes)	0
			
	Q62	Path/Module 2 (gene centric)	
*** MOD2.1	Q7	What genes are implicated in [condition]?	3
MOD2.2	Filter	What subset of genes are most representative of [conditions]?  (find archetypes)	0
*** MOD2.3		What pathways/processes are [genes] involved in?	2/3
*** MOD2.4	Q4	What genes are involved in [pathway/process]?	2/3
*** MOD2.5	Q9	What drugs/compounds target gene products of [gene]?	3
MOD2.6	?	What [non human genes] are similar to [human gene]? (Can be phenotypic, functional, or structural similarity)	
			
		Path/Module 3 (agent-centric)	
MOD3.1		What toxicants (chemical agent / toxic protein) does [condition] have specific sensitivity to?	
*** MOD3.2		What proteins produce agent [x]?	3
*** MOD3.3		What drugs/compounds decrease (inhibit / down-regulate) these?	3
MOD3.2a		What proteins consume/sequester agent [x]?	2
*** MOD3.3a		What drugs increase (activate / promote ) these?	3
MOD3.2b		What phenotypes / conditions are associated with high exposure to [x]?	1
MOD3.3b		What drugs rarely produce these adverse events? (possibly protective effect)	0/2
		(the end of the agent-centric path continues to the annotations path (component 6)	
			
		Annotations Path	
PathA.1		What are the common side effects of [drug]?	2
PathA.2		What is the approval status of [drug]?	2
PathA.3		What are the approved indications for [drug]?	2
PathA.4		What is the established pharm class for [drug]?	2
			
		Scoring Path	
PathS.1		Does the drug (or a metabolite) get to the target tissues / cell-types?	0/1/2
PathS.2		Is the drug approved / in clinical trials?	2
PathS.3		Is the drug relatively non-toxic?	2
PathS.4		Is the drug already being investigated for the same/similar condition?	2
PathS.5		Does the drug have favorable intellectual property (IP) status?	2
PathS.6		Does the drug have available analogs?	2
PathS.7		Do any analogs of the drug satisfy 10-15 above?	2
			
		Drug Set Expansion Path	
PathD.1		identify defining features	0
PathD.2		find groups	0
|#
#lang racket

(provide
  (all-from-out "mk.rkt")
  (all-from-out "mk-db.rkt")
  (all-defined-out))

(require
  "mk.rkt"
  "mk-db.rkt"
  (except-in racket/match ==))

(displayln "loading semmed")
(define semmed (time (make-db "data/semmed")))
(displayln "loading monarch-lite")
(define monarch (time (make-db "data/monarch-lite")))
(displayln "loading rtx")
(define rtx (time (make-db "data/rtx")))

;; list membership
(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        [(== x y)]
        [(=/= x y) (membero x rest)]))))

;; remove duplicates from a list
(define rem-dups
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(member (car ls) (cdr ls)) (rem-dups (cdr ls))]
      [else (cons (car ls) (rem-dups (cdr ls)))])))


(define (edgeo e)
  (conde
    ((fresh (ee) (== `(semmed . ,ee) e) (db:edgeo semmed ee)))
    ((fresh (ee) (== `(monarch . ,ee) e) (db:edgeo monarch ee)))
    ((fresh (ee) (== `(rtx . ,ee) e) (db:edgeo rtx ee)))
    ))

(define (~cui-concepto cui c)
  (conde
    ((fresh (cc) (== `(semmed . ,cc) c) (db:~cui-concepto semmed cui cc)))
    ((fresh (cc) (== `(monarch . ,cc) c) (db:~cui-concepto monarch cui cc)))
    ((fresh (cc) (== `(rtx . ,cc) c) (db:~cui-concepto rtx cui cc)))
    ))

(define (~name-concepto name c)
  (conde
    ((fresh (cc) (== `(semmed . ,cc) c) (db:~name-concepto semmed name cc)))
    ((fresh (cc) (== `(monarch . ,cc) c) (db:~name-concepto monarch name cc)))
    ((fresh (cc) (== `(rtx . ,cc) c) (db:~name-concepto rtx name cc)))
    ))

(define (concepto c)
  (conde
    ((fresh (cc) (== `(semmed . ,cc) c) (db:concepto semmed cc)))
    ((fresh (cc) (== `(monarch . ,cc) c) (db:concepto monarch cc)))
    ((fresh (cc) (== `(rtx . ,cc) c) (db:concepto rtx cc)))
    ))

(define get-concepts-from-cui
  (lambda (cui)
    (run* (concept)
      (~cui-concepto cui concept))))


(define (DECREASES pred)
  (fresh (_)
    (conde
      [(== `(,_ . "negatively_regulates") pred)]
      [(== `(,_ . "prevents") pred)]
      [(== `(,_ . "treats") pred)])))

(define (INCREASES pred)
  (fresh (_)
    (conde
      [(== `(,_ . "positively_regulates") pred)]
      [(== `(,_ . "causes") pred)]
      [(== `(,_ . "produces") pred)])))

;; 	Q62	Path/Module 2 (gene centric)	
;; *** MOD2.1	Q7	What genes are implicated in [condition]?	3
;; MOD2.2	Filter	What subset of genes are most representative of [conditions]?  (find archetypes)	0
;; *** MOD2.3		What pathways/processes are [genes] involved in?	2/3
;; *** MOD2.4	Q4	What genes are involved in [pathway/process]?	2/3
;; *** MOD2.5	Q9	What drugs/compounds target gene products of [gene]?	3
;; forget it   MOD2.6	?	What [non human genes] are similar to [human gene]? (Can be phenotypic, functional, or structural similarity)	


;; *** MOD2.1	Q7	What genes are implicated in [condition]?	3

;; get a list of conditions from the JSON blob:
;;
;;   "result_list": [
;;      "result_graph": {
;;          "node_list": [
;;
;; for this particular example, for each result_list, we just pick the
;; node in the node_list that has "type": "disease" and which has the OMIM ID (as
;; opposed to the DOID ID): "id": "OMIM:600001"


;; example condition: 
;; "source_id": "OMIM:600001",





;; *** MOD2.1	Q7	What genes are implicated in [condition]?	3

#|
(define condition-concepts (rem-dups (get-concepts-from-cui "OMIM:606176")))
|#

;; causes_or_contributes_to

;; protein

#|
;;; get list of proteins from edges 'protein X causes_or_contributes_to disease Y'
(define proteins-causing-condition
  (run 50 (proteins)
    (fresh (dbname eid s o pid pred eprops
                   scid scui sname scatid scat sprops
                   ocid ocui oname ocatid ocat oprops
                   e
                   cond-con)
      (== `(,dbname . ,s) proteins)
      (membero cond-con condition-concepts)
      (== `(,dbname . ,o) cond-con)
      (== "causes_or_contributes_to" pred)
      (== "protein" scat)
      (== `(,scid ,scui ,sname (,scatid . ,scat) . ,sprops) s)
      (== `(,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops) o)
      (== `(,dbname ,eid ,s ,o (,pid . ,pred) . ,eprops) e)
      (edgeo e))))
|#

;; MOD2.2	Filter	What subset of genes are most representative of [conditions]?  (find archetypes)	0

;; ignore for now--null/identity filter!


;; *** MOD2.3		What pathways/processes are [genes] involved in?	2/3

;; rtx
;; protein           36056 "UniProtKB:Q14654" "KCNJ11" (1 . "protein")
;; participates_in   (2 . "participates_in")
;; pathway           (115579 "REACT:R-HSA-199992" "trans-Golgi Network Vesicle Budding" (8 . "pathway")

#|
(define pathways-involved-in-genes-causing-condition
  (rem-dups
    (run 50 (pathway)
      (fresh (dbname eid s o pid pred eprops
                     scid scui sname scatid scat sprops
                     ocid ocui oname ocatid ocat oprops
                     e
                     protein)
        (membero protein proteins-causing-condition)
        (== `(,dbname . ,o) pathway)
        (== `(,dbname . ,s) protein)
        (== "protein" scat)
        (== "participates_in" pred)
        (== "pathway" ocat)
        (== `(,scid ,scui ,sname (,scatid . ,scat) . ,sprops) s)
        (== `(,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops) o)
        (== `(,dbname ,eid ,s ,o (,pid . ,pred) . ,eprops) e)
        (edgeo e)))))
|#

;; *** MOD2.4	Q4	What genes are involved in [pathway/process]?	2/3

#|
(define proteins-involved-in-pathways-of-interest
  (rem-dups
    (run 50 (protein)
      (fresh (dbname eid s o pid pred eprops
                     scid scui sname scatid scat sprops
                     ocid ocui oname ocatid ocat oprops
                     e
                     pathway)
        (membero pathway pathways-involved-in-genes-causing-condition)
        (== `(,dbname . ,o) pathway)
        (== `(,dbname . ,s) protein)
        (== "protein" scat)
        (== "participates_in" pred)
        (== "pathway" ocat)
        (== `(,scid ,scui ,sname (,scatid . ,scat) . ,sprops) s)
        (== `(,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops) o)
        (== `(,dbname ,eid ,s ,o (,pid . ,pred) . ,eprops) e)
        (edgeo e)))))
|#

;; *** MOD2.5	Q9	What drugs/compounds target gene products of [gene]?	3

;; mygene.info to convert Uniprot protein to UMLS gene (perhaps through HGNC:HGNC:795)

;; then, change the UMLS term for the drug to the Chembl id

;; punt!  use fuzzy lookup in semmed, based on protein name from rtx, looking for gene in semmed

#|
(define semmed-genes
  (rem-dups
    (run 50 (gene)
      (fresh (procid procui proname procatid procat proprops
                     scid scui sname scatid sprops
                     protein)
        (membero protein proteins-involved-in-pathways-of-interest)
        (== `(rtx ,procid ,procui ,proname (,procatid . ,procat) . ,proprops) protein)
        (== `(semmed ,scid ,scui ,sname (,scatid . "gene") . ,sprops) gene)
        (~name-concepto proname gene)))))
|#

#|
(define semmed-drugs
  (rem-dups
    (run 50 (drug)
      (fresh (dbname eid s o pid pred eprops
                     scid scui sname scatid scat sprops
                     ocid ocui oname ocatid ocat oprops
                     e
                     gene)
        (membero gene semmed-genes)
        (== `(semmed . ,s) drug)
        (== `(semmed . ,o) gene)
        (== "chemical_substance" scat)
        (conde
          [(== "positively_regulates" pred)]
          [(== "negatively_regulates" pred)])
        (== `(,scid ,scui ,sname (,scatid . ,scat) . ,sprops) s)
        (== `(,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops) o)
        (== `(semmed ,eid ,s ,o (,pid . ,pred) . ,eprops) e)
        (edgeo e)))))
|#

#|
(define rtx-drugs
  (rem-dups
    (run 20 (rtx-drug)
      (fresh (semmed scid scui sname scatid scat sprops
                     rcid rcui rname rcatid rprops
              semmed-drug)
        (membero semmed-drug semmed-drugs)
        (== `(semmed ,scid ,scui ,sname (,scatid . ,scat) . ,sprops) semmed-drug)
        (== `(rtx ,rcid ,rcui ,rname (,rcatid . "chemical_substance") . ,rprops) rtx-drug)
        (~name-concepto sname rtx-drug)))))
|#

;; positively_regulates
;; negatively_regulates
;; semmed  (5 . chemical_substance)

;; fuzzy search back to rtx
;; (10 . chemical_substance)



(define workflow1module2
  (lambda (my-cui)
    (define condition-concepts (rem-dups (get-concepts-from-cui my-cui)))
    ;; *** MOD2.1	Q7	What genes are implicated in [condition]?	3
    ;;(printf "condition-concepts\n")
    (define proteins-causing-condition
      (run 50 (proteins)
        (fresh (dbname eid s o pid pred eprops
                       scid scui sname scatid scat sprops
                       ocid ocui oname ocatid ocat oprops
                       e
                       cond-con)
          (== `(,dbname . ,s) proteins)
          (membero cond-con condition-concepts)
          (== `(,dbname . ,o) cond-con)
          (== "causes_or_contributes_to" pred)
          (== "protein" scat)
          (== `(,scid ,scui ,sname (,scatid . ,scat) . ,sprops) s)
          (== `(,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops) o)
          (== `(,dbname ,eid ,s ,o (,pid . ,pred) . ,eprops) e)
          (edgeo e))))
    ;; MOD2.2	Filter	What subset of genes are most representative of [conditions]?  (find archetypes)	0
    ;; ignore for now--null/identity filter!

    ;;(printf "pathways-involved-in-genes-causing-condition\n")
    
    ;; *** MOD2.3		What pathways/processes are [genes] involved in?	2/3    
    (define pathways-involved-in-genes-causing-condition
      (rem-dups
       (run 50 (pathway)
         (fresh (dbname eid s o pid pred eprops
                        scid scui sname scatid scat sprops
                        ocid ocui oname ocatid ocat oprops
                        e
                        protein)
           (membero protein proteins-causing-condition)
           (== `(,dbname . ,o) pathway)
           (== `(,dbname . ,s) protein)
           (== "protein" scat)
           (== "participates_in" pred)
           (== "pathway" ocat)
           (== `(,scid ,scui ,sname (,scatid . ,scat) . ,sprops) s)
           (== `(,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops) o)
           (== `(,dbname ,eid ,s ,o (,pid . ,pred) . ,eprops) e)
           (edgeo e)))))

    (printf "proteins-involved-in-pathways-of-interest")
        
    ;; *** MOD2.4	Q4	What genes are involved in [pathway/process]?	2/3
    (define proteins-involved-in-pathways-of-interest
      (rem-dups
       (run 50 (protein)
         (fresh (dbname eid s o pid pred eprops
                        scid scui sname scatid scat sprops
                        ocid ocui oname ocatid ocat oprops
                        e
                        pathway)
           (membero pathway pathways-involved-in-genes-causing-condition)
           (== `(,dbname . ,o) pathway)
           (== `(,dbname . ,s) protein)
           (== "protein" scat)
           (== "participates_in" pred)
           (== "pathway" ocat)
           (== `(,scid ,scui ,sname (,scatid . ,scat) . ,sprops) s)
           (== `(,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops) o)
           (== `(,dbname ,eid ,s ,o (,pid . ,pred) . ,eprops) e)
           (edgeo e)))))
    ;; *** MOD2.5	Q9	What drugs/compounds target gene products of [gene]?	3
    (define semmed-genes
      (rem-dups
       (run 50 (gene)
         (fresh (procid procui proname procatid procat proprops
                        scid scui sname scatid sprops
                        protein)
           (membero protein proteins-involved-in-pathways-of-interest)
           (== `(rtx ,procid ,procui ,proname (,procatid . ,procat) . ,proprops) protein)
           (== `(semmed ,scid ,scui ,sname (,scatid . "gene") . ,sprops) gene)
           (~name-concepto proname gene)))))
    (define semmed-drugs
      (rem-dups
       (run 50 (drug)
         (fresh (dbname eid s o pid pred eprops
                        scid scui sname scatid scat sprops
                        ocid ocui oname ocatid ocat oprops
                        e
                        gene)
           (membero gene semmed-genes)
           (== `(semmed . ,s) drug)
           (== `(semmed . ,o) gene)
           (== "chemical_substance" scat)
           (conde
             [(== "positively_regulates" pred)]
             [(== "negatively_regulates" pred)])
           (== `(,scid ,scui ,sname (,scatid . ,scat) . ,sprops) s)
           (== `(,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops) o)
           (== `(semmed ,eid ,s ,o (,pid . ,pred) . ,eprops) e)
           (edgeo e)))))    
    (define rtx-drugs
      (rem-dups
       (run 20 (rtx-drug)
         (fresh (semmed scid scui sname scatid scat sprops
                        rcid rcui rname rcatid rprops
                        semmed-drug)
           (membero semmed-drug semmed-drugs)
           (== `(semmed ,scid ,scui ,sname (,scatid . ,scat) . ,sprops) semmed-drug)
           (== `(rtx ,rcid ,rcui ,rname (,rcatid . "chemical_substance") . ,rprops) rtx-drug)
           (~name-concepto sname rtx-drug)))))
    rtx-drugs))

;; (pretty-print (workflow1module2 "OMIM:606176"))
