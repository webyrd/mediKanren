#lang racket
(require "../../medikanren/pieces-parts/query.rkt")

#|
This file focuses on migraine conditions as an example case, but these functions can be applied
to many different conditions.

The functions in this file can be used to find pathways involved in diseases/conditions that are
not yet targeted by drug treatments. This information can be useful for drug discovery or finding
drug repurposing candidates.

It can also be used for patient cases. It can help determine what pathways were targeted by
treatments that were or weren't effective for a patient. Then, if a treatment was effective but
caused many side effects, the user can look for another drug that targets the same pathway in
the same way, and may be as effective whithout as many side effects. Or if no treatments have
been effective, the user can look for pathways that the patient has not yet tried to target
and can see if there are any available treatments that target those pathways.

KGs loaded*:
uab-pmi
semmed
umlsmeta
orange
robokop
rtx2
covid19
textminingprovider
pr-owl
sri_semmeddb
sri-reference-kg

*Although loading all of these is more comprehensive, rtx2 is necessary and sufficient for
getting some useful results.
|#

#|
Helpful functions for reporting and viewing results
|#
(define (unwrap lst)
  (cond [(or (null? lst) (not (list? lst))) lst]
        [(list? (car lst)) (append (car lst) (unwrap (cdr lst)))]
        [else (cons (car lst) (unwrap (cdr lst)))]))

(define (curie->curie/name curie)
  (assoc curie (curie-synonyms/names curie)))

(define (concept->curie/name concept)
  (cons (concept->curie concept) (concept->name concept)))

(define (edges->preds edges)
  (remove-duplicates (map edge->pred edges)))

(define (summarize-curie curie)
  (let ([concepts (find-concepts #t (list curie))])
    (map (lambda (c) (list (concept->dbname c) (concept->curie c) (concept->name c) (concept->category c))) concepts)))

#|
Redefining category filters. The ones in query.rkt don't include "biolink" categories
|#
(define gene-or-protein '(;; rtx2
                          "biolink:Protein"
                          "biolink:GeneProduct"
                          "biolink:Gene"
                          "biolink:GeneFamily"
                          "biolink:GenomicEntity"
                          ;; semmed
                          "gene"
                          ;; robokop
                          "(\"named_thing\" \"gene\")"
                          ;; orange
                          "(\"gene\")"
                          "(\"genomic entity\" \"gene\")"
                          "(\"gene\" \"genomic entity\")"
                          ;; semmed
                          "biological_entity" ;; with \"Amino Acid, Peptide, or Protein\" in "umls_type_label"
                          ;; robokop
                          ;; orange
                          "(\"gene\" \"genomic entity\")"))

(define drug '(;; rtx2
               "biolink:Drug"
               "biolink:ChemicalSubstance"
               ;; semmed
               "chemical_substance"
               ;; robokop
               "(\"named_thing\" \"chemical_substance\")"))


;;------------------------------------------Migraine case-------------------------------------------------

#|
Defining all parent migraine curies found using find-concepts, filtering by categories, and manual
filtering and checking of heirarchy
|#
;; UMLS
(define migraine-disorders-umls "UMLS:C0149931") ;; has same supers as migraine-disorders-cui
(define migraine-variant-umls "UMLS:C0042331") ;; suclass_of -> CUI-supers
(define common-migraine-umls "UMLS:C0338480")
(define migraine-with-aura-umls "UMLS:C0154723")

;; HP
(define migraine-hp "HP:0002076")

;; DOID
(define migraine-disorder-doid "DOID:6364")

;; MONDO
(define migraine-disorder-mondo "MONDO:0005277")

;; OMIM
(define migraine-omim "OMIM:157300")

#|
Compiles all curies into one list
|#
(define migraine-parent-curies (list migraine-disorders-umls
                                    migraine-variant-umls
                                    common-migraine-umls
                                    migraine-with-aura-umls
                                    migraine-hp
                                    migraine-disorder-doid
                                    migraine-disorder-mondo
                                    migraine-omim))

#|
Gets the concepts associated with all the parent curies
|#
(define migraine-parent-concepts (find-concepts #t migraine-parent-curies))

#|
Given a list of disease concepts, this uses run/graph to find the drugs that are indicated for
or used to treat/prevent the disease concepts. It is meant to be run for one disease at a time,
but it allows for the input of a list of disease concepts because you often need multiple concepts
to comprehensively represent a disease for the query.

Returns a list of the drug to disease edges from the query. The drug concepts can be extracted
using calls to (edge->subject), and the curies can be extracted using (edge->subject) and
(concept->curie).
|#
(define (drugs-indicated-for-r/g disease-concepts)
  (define treats (find-exact-predicates '("biolink:treats"
                                          "treats"
                                          "biolink:prevents"
                                          "prevents"
                                          )))
  (match-define
    (list concepts edges)
    (run/graph
     ((Dr #f)
      (Dis disease-concepts))
     ((Dr->Dis treats))
     (Dr Dr->Dis Dis)))
  
  (hash-ref edges 'Dr->Dis))

#|
Does the same thing as (drugs-indicated-for-r/g) except it takes a list of curies rather than
concepts and uses query/graph rather than run/graph.

Returns the same thing as (drugs-indicated-for-r/g), a list of drug to disease edges.
|#
(define (drugs-indicated-for-q/g disease-curies)
  (define drug-lst '())
  (define treats '("biolink:treats"
                   "treats"))
  (for-each
   (lambda (d)
     (define q (query/graph
                ((Dr drug)
                 (Dis d))
                ((Dr->Dis treats))
                (Dr Dr->Dis Dis)))
     (set! drug-lst (set-union drug-lst (edges/query q 'Dr->Dis))))
   disease-curies)
  drug-lst
  )

#|
Takes in a list of drug concepts and uses run/graph to find the biological pathways those drugs
target.

Returns a list of the drug to pathway edges. Pathway concepts can be extracted using (edge->object),
and the pathway curies can be extracted using (edge->object) and (concept->curie).
|#
(define (find-drug-target-pathways-r/g drug-concepts)
  (define participates-in (find-predicates '("biolink:participates_in")))
  (match-define
    (list concepts edges)
    (run/graph
     ((D drug-concepts)
      (P #f))
     ((D->P participates-in))
     (D D->P P)))
  (hash-ref edges 'D->P))

#|
Does the same thing as (find-drug-target-pathways-r/g) except it takes a list of drug curies instead
of concepts and uses query/graph instead of run/graph.

Returns the same thing as (find-drug-target-pathways-r/g), a list of drug to pathway edges.
|#
(define (find-drug-target-pathways-q/g drug-curies)
  (define pathway-lst '())
  (for-each
   (lambda (d)
     (define q (query/graph
                ((D d)
                 (P #f))
                ((D->P '("biolink:participates_in")))
                (D D->P P)))
     (set! pathway-lst (set-union pathway-lst (edges/query q 'D->P))))
   drug-curies)
  pathway-lst)

#|
Takes a list of pathway concepts and uses run/graph to find the proteins that participate in that
pathway.

Returns a list of protein to pathway edges.
|#
(define (find-prots-in-pathways pathway-concepts)
  (define participates-in (find-predicates '("biolink:participates_in")))
  (match-define
    (list concepts edges)
    (run/graph
     ((U #f)
      (P pathway-concepts))
     ((U->P participates-in))
     (U U->P P)))
  
  (filter (lambda (e)
            (string-prefix? (concept->curie (edge->subject e))
                            "UniProtKB"))
          (hash-ref edges 'U->P)))

#|
Takes a list of protein concepts and uses run/graph to find all the pathways the proteins participate in.

Returns a list of protein to pathway edges.
|#
(define (find-pathways-invovled-in prot-concepts)
  (define participates-in (find-predicates '("biolink:participates_in")))
  (match-define
    (list concepts edges)
    (run/graph
     ((U prot-concepts)
      (P #f))
     ((U->P participates-in))
     (U U->P P)))
  
  (filter (lambda (e)
            (string-prefix? (concept->curie (edge->object e))
                            "PathWhiz"))
          (hash-ref edges 'U->P)))
  
#|
Given a list of disease curies, uses query/graph to find the genes or proteins associated with that
disease/condition.

Returns a list of gene/protein to disease edges.
|#
(define (genes/prots-assoc-with-disease disease-curies)
  (define associated-with '(;; UniProt -> DOID, MONDO, OMIM
                            "biolink:correlated_with"
                            ;; UniProt -> DOID
                            "biolink:gene_associated_with_condition"
                            ;; UniProt -> HP
                            "biolink:has_phenotype"
                            ;; " -> OMIM, MONDO
                            "biolink:causes"
                            "biolink:participates_in"
                            ;;
                            "predisposes"
                            "has_phenotype"
                            "contributes_to"
                            "causes"
                            "gene_associated_with_condition"
                            "biomarker_for"))
  (define gene/prot-lst '())
  (for-each
   (lambda (d)
     (define q (query/graph
                ((G/P gene-or-protein)
                 (Dis d))
                ((G/P->Dis associated-with))
                (G/P G/P->Dis Dis)))
     (set! gene/prot-lst (set-union gene/prot-lst (edges/query q 'G/P->Dis)))
     )
   disease-curies)
  gene/prot-lst
  )

#|
Takes a list of biological pathway curies and uses query/graph to find the drugs that target them.

Returns a list of drug to pathway edges.
|#
(define (find-drugs-for-pathways pathway-curies)
  (define pathway-lst '())
  (for-each
   (lambda (p)
     (define q (query/graph
                ((D drug)
                 (P p))
                ((D->P '("biolink:participates_in")))
                (D D->P P)))
     (set! pathway-lst (set-union pathway-lst (edges/query q 'D->P))))
   pathway-curies)
  pathway-lst)

#|Example
;;Drugs that treat migraines
(define drug->migraine (drugs-indicated-for-q/g migraine-parent-curies))
(define migraine-drug-concepts (map edge->subject drug->migraine))
(define migraine-drug-curies (map concept->curie (map edge->subject drug->migraine)))

;;Pathways targeted by drugs
(define drug->target-pathways (find-drug-target-pathways-q/g migraine-drug-curies))
(define drug-pathway-concepts (map edge->object drug->target-pathways))
(define drug-pathway-curies (map concept->curie (map edge->object drug->target-pathways)))

;;Proteins involved in pathways targeted by drugs
(define prots->drug-pathways (find-prots-in-pathways drug-pathway-concepts))
(define prots-in-drug-pathways-concepts (map edge->subject prots->drug-pathways))
(define prots-in-drug-pathways-curies (remove-duplicates (map concept->curie (map edge->subject prots->drug-pathways))))

;;Genes/prots associated with migraines
(define g/p->disease (genes/prots-assoc-with-disease migraine-parent-curies))
(define g/p-concepts (map edge->subject g/p->disease))
(define g/p-curies (map concept->curie (map edge->subject g/p->disease)))

;;Pathways involved in migraine conditions
(define g/p->disease-pathways (find-pathways-invovled-in g/p-concepts))
(define disease-pathway-concepts (map edge->object g/p->disease-pathways))
(define disease-pathway-curies (map concept->curie (map edge->object g/p->disease-pathways)))

;;Cross-referencing disease and drug pathways
(define targeted-pathways (set-intersect drug-pathway-curies disease-pathway-curies))
(define non-targeted-pathways (set-subtract disease-pathway-curies drug-pathway-curies))

(map curie->curie/name targeted-pathways)
(map curie->curie/name non-targeted-pathways)

;;Find drugs for pathways not yet targeted by migraine drugs
(define repurposing-candidates->pathways (find-drugs-for-pathways non-targeted-pathways))
(define drug-repurposing-candidate-concepts (map edge->subject repurposing-candidates->pathways))
(define drug-repurposing-candidate-curies (map concept->curie (map edge->subject repurposing-candidates->pathways)))
|#