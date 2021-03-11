#lang racket
(provide (all-defined-out)
         (all-from-out "../../medikanren/common.rkt" "../../medikanren/pieces-parts/mk-db.rkt"))
(require "../../medikanren/pieces-parts/common.rkt" "../../medikanren/pieces-parts/mk-db.rkt")
(require racket/date)
(require csv-reading)
(require csv-writing)
(require "../../medikanren/pieces-parts/csv.rkt"
         "../../medikanren/pieces-parts/repr.rkt")
(require racket/list racket/port racket/set racket/stream racket/string)
(provide set-field-separator!
         csv-records)

(define extract-name/curie/category-from-concept-ls
  (lambda (query-ls els)
    (cond
      ((null? query-ls) els)
      ((or (void? (car query-ls))
           (boolean? (car query-ls)))
       (extract-name/curie/category-from-concept-ls
        (cdr query-ls) els))
      (else 
       (match (car query-ls)
         [`(,db ,cui ,id ,name ,category ,properties-list)
          (extract-name/curie/category-from-concept-ls
           (cdr query-ls)
           (cons
            (list db id name category) els))])))))


(define extract-curie-from-concept-ls
  (lambda (query-ls els)
    (cond
      ((null? query-ls) els)
      ((or (void? (car query-ls))
           (boolean? (car query-ls)))
       (extract-curie-from-concept-ls
        (cdr query-ls) els))
      (else 
       (match (car query-ls)
         [`(,db ,cui ,id ,name ,category ,properties-list)
          (extract-curie-from-concept-ls
           (cdr query-ls)
           (cons
            (list id) els))])))))


;; no rtx2 categories
(define molecular_entity
  '((semmed 2 . "gene")
    (orange 6 . "(\"gene\")")
    (orange 19 . "(\"genomic entity\" \"gene\")")
    (orange 23 . "(\"gene\" \"genomic entity\")")
    (robokop 0 . "(\"named_thing\" \"gene\")")	      
    (robokop 11 . "(\"named_thing\" \"gene_family\")")
    (semmed 6 . "protein")
    (rtx 1 . "protein")
    (orange 28 . "(\"genome\")")
    (orange 22 . "(\"genomic entity\")")
    (semmed 9 . "genomic_entity")
    (rtx 2 . "microRNA")
    (orange 15 . "(\"transcript\")")
    (rtx 3 . "metabolite")
    (semmed 0 ."biological_entity")
    (orange 12 . "(\"food material\")")))

(define member?
  (lambda (a ls)
    (cond
      ((null? ls) #f)
      (else
       (or (equal? a (car ls))
           (member? a (cdr ls)))))))

(define filter-concept-by-f
  (lambda (ls els curie f)
    (cond
      ((null? ls) els)
      ((f curie (car ls))
       (filter-concept-by-f
        (cdr ls)
        (cons (car ls) els) curie f))
      (else
       (filter-concept-by-f
        (cdr ls) els curie f)))))

(define HP-symptoms-from-input/concept-ls
  (filter-concept-by-f
   (find-concepts
    #t
    (list
     "HP:0001531"
     "HP:0003550"
     "HP:0001004"
     "HP:0008513"
     "HP:0002144"
     "HP:0001790")) '() 'orange member?))

#|
(define HP-symptoms-from-input/concept-ls
  (find-concepts
    #t
    (list
     "HP:0001531"
     "HP:0003550"
     "HP:0001004"
     "HP:0008513"
     "HP:0002144"
     "HP:0001790")))
|#

(newline)
(displayln (format "CONCEPTS FOUND RETURNED FROM INITIAL INPUT CURIE:"))
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls HP-symptoms-from-input/concept-ls '()))

(define subclass_of (find-exact-predicates (list "subclass_of")))
(define contributes_to (find-exact-predicates (list "contributes_to")))
(define causally_related_to (find-exact-predicates (list "causually_related_to")))
(define has_phenotype (find-exact-predicates (list "has_phenotype")))

;;subclass of doesn't give any more concepts
;;will attempt to get disease assoc next 


#|
(match-define
 (list A-->subclass_of-->HP-input=>concepts
       A-->subclass_of-->HP-input=>edges)
 (time
  (run/graph
   ((A #f)
    (HP-input HP-symptoms-from-input/concept-ls))
   ((--subclass_of--> subclass_of))
   (A --subclass_of--> HP-input))))
|# 

;;orange only
(match-define
 (list A-->subclass_of-->HP-input=>concepts
       A-->subclass_of-->HP-input=>edges)
 (time
  (run/graph
   ((A #f)
    (HP-input HP-symptoms-from-input/concept-ls))
   ((--subclass_of--> '((orange 0 . "subclass_of"))))
   (A --subclass_of--> HP-input))))

(define A-->subclass_of-->HP-input/concepts
  (hash-ref A-->subclass_of-->HP-input=>concepts 'A))

(define HP-concept-ls
  (set-union A-->subclass_of-->HP-input/concepts
             HP-symptoms-from-input/concept-ls))

(newline)
(displayln (format "~a CONCEPTS FROM:\nA --subclass_of--> HP-concept-ls" (length HP-concept-ls)))
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls HP-concept-ls '()))
(newline)

#|
(match-define
 (list A--has_phenotype-->HP-ls=>concepts
       A--has_phenotype-->HP-ls=>edges)
 (time
  (run/graph
   ((A #f)
    (HP-ls HP-concept-ls))
   ((--has_phenotype--> has_phenotype))
   (A --has_phenotype--> HP-ls))))
|#

;;orange only
(match-define
 (list A--has_phenotype-->HP-ls=>concepts
       A--has_phenotype-->HP-ls=>edges)
 (time
  (run/graph
   ((A #f)
    (HP-ls HP-concept-ls))
   ((--has_phenotype--> '((orange 2 . "has_phenotype"))))
   (A --has_phenotype--> HP-ls))))

(define A-->has_phenotype-->HP-input/concepts
  (hash-ref A--has_phenotype-->HP-ls=>concepts 'A))

(define A-->has_phenotype-->HP-input/edges
  (hash-ref A--has_phenotype-->HP-ls=>edges '--has_phenotype-->))

(newline)
(displayln (format "~a CONCEPTS FROM:\nA --has_phenotype--> HP-concept-ls" (length A-->has_phenotype-->HP-input/concepts)))
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls A-->has_phenotype-->HP-input/concepts '()))
(newline)

#|
(match-define
 (list B--contributes_to-->A--has_phenotype-->HP-ls=>concepts
       B--contributes_to-->A--has_phenotype-->HP-ls=>edges)
 (time
  (run/graph
   ((A #f)
    (B molecular_entity)
    (HP-ls HP-concept-ls))
   ((--has_phenotype--> has_phenotype)
    (--contributes_to--> contributes_to))
   (B --contributes_to--> A --has_phenotype--> HP-ls))))
|#


(match-define
 (list B--contributes_to-->A--has_phenotype-->HP-ls=>concepts
       B--contributes_to-->A--has_phenotype-->HP-ls=>edges)
 (time
  (run/graph
   ((A #f)
    (B #f)
    (HP-ls HP-concept-ls))
   ((--has_phenotype--> '((orange 2 . "has_phenotype")))
    (--contributes_to--> '((orange 11 . "contributes_to"))))
   (B --contributes_to--> A --has_phenotype--> HP-ls))))

(define B--contributes_to-->A--has_phenotype-->HP-ls/concepts 
  (hash-ref B--contributes_to-->A--has_phenotype-->HP-ls=>concepts 'B))
 
(define B--contributes_to-->A--has_phenotype-->HP-ls/edges
  (hash-ref B--contributes_to-->A--has_phenotype-->HP-ls=>edges '--contributes_to-->)) 

(newline)
(displayln (format "~a CONCEPTS FROM:\nB--contributes_to-->A--has_phenotype-->HP-ls/concepts" (length B--contributes_to-->A--has_phenotype-->HP-ls/concepts)))
(newline)
(pretty-print (extract-name/curie/category-from-concept-ls B--contributes_to-->A--has_phenotype-->HP-ls/concepts '()))
(newline)

(define NCBIGenes-from-symptoms/orange
  (flatten (extract-curie-from-concept-ls B--contributes_to-->A--has_phenotype-->HP-ls/concepts '())))

(define NCBIGenes-from-symptoms/orange/concept-ls
  (find-concepts #t NCBIGenes-from-symptoms/orange))

;; NCBIGene from orange --> HGNC gene in robokop/rtx2

(match-define
 (list NCBIGene_input--equivalent_to-->HGNC--encodes-->UniProtKB=>concepts
       NCBIGene_input--equivalent_to-->HGNC--encodes-->UniProtKB=>edges)
 (time
  (run/graph
   ((A #f)
    (B #f)
    (NCBIGene_input NCBIGenes-from-symptoms/orange/concept-ls)) 
   ((--equivalent_to--> '((rtx2 57 . "equivalent_to")))
    (--encodes--> '((rtx2 775 . "encodes"))))
   (NCBIGene_input --equivalent_to--> A --encodes--> B))))

(define NCBIGene_input--equivalent_to-->HGNC/concepts
  (hash-ref NCBIGene_input--equivalent_to-->HGNC--encodes-->UniProtKB=>concepts 'A))
(define NCBIGene_input--equivalent_to-->HGNC--encodes-->UniProtKB/concepts
  (hash-ref NCBIGene_input--equivalent_to-->HGNC--encodes-->UniProtKB=>concepts 'B))
 
(define HGNC-from-symptoms
  (flatten (extract-curie-from-concept-ls NCBIGene_input--equivalent_to-->HGNC/concepts '())))

(define HGNC-from-symptoms/concept-ls
  (find-concepts #t HGNC-from-symptoms))

(define UniProtKB-from-symptoms
  (flatten (extract-curie-from-concept-ls NCBIGene_input--equivalent_to-->HGNC--encodes-->UniProtKB/concepts '())))

(define UniProtKB-from-symptoms/concept-ls
  (find-concepts #t UniProtKB-from-symptoms))


;; rtx2 uniprotkb --> GO

;; GO is 3 separate ontologies: Cellular Component, Biological Process, Molecular Function
;; isa predicate operates within one ontology and will not cross
;; part_of , regulates cross between the ontologies
;; isa --> biological_process GO ontology
;; biological process is broken into 3 nodes = cellular process, development, physiologic process
;; regulates repdicate not really useful 
#|
(match-define
 (list UniProtKB_input--involved_in-->GO--regulates-->GOparent=>concepts
       UniProtKB_input--involved_in-->GO--regulates-->GOparent=>edges)
 (time
  (run/graph
   ((A #f)
    (B #f)
    (UniProtKB_input UniProtKB-from-symptoms/concept-ls)) 
   ((--involved_in--> '((rtx2 182 . "involved_in"))) 
    (--regulates--> '((rtx2 185 . "regulates"))))
   (UniProtKB_input --involved_in--> A --regulates--> B))))

(define UniProtKB_input--involved_in-->GO/concepts
  (hash-ref UniProtKB_input--involved_in-->GO--regulates-->GOparent=>concepts 'A))
(define UniProtKB_input--involved_in-->GO--regulates-->GOparent/concepts
  (hash-ref UniProtKB_input--involved_in-->GO--regulates-->GOparent=>concepts 'B))
(define UniProtKB_input--involved_in-->GO/edges
  (hash-ref UniProtKB_input--involved_in-->GO--regulates-->GOparent=>edges '--involved_in-->))
(define UniProtKB_input--involved_in-->GO--regulates-->GOparent/edges 
  (hash-ref UniProtKB_input--involved_in-->GO--regulates-->GOparent=>edges '--regulates-->))

;;rtx2 involved_in gives regulation/positive/negative terms back
(pretty-print (extract-name/curie/category-from-concept-ls UniProtKB_input--involved_in-->GO/concepts '()))
 
;; isa--> "biological process"
(pretty-print (extract-name/curie/category-from-concept-ls UniProtKB_input--involved_in-->GO--regulates-->GOparent/concepts '()))
|#

;; use to get 
(rtx2 188 . "positively_regulates")
(rtx2 189 . "negatively_regulates")

(match-define
 (list UniProtKB_input--involved_in-->GO--part_of-->GOparent=>concepts
       UniProtKB_input--involved_in-->GO--part_of-->GOparent=>edges)
 (time
  (run/graph
   ((A #f)
    (B #f)
    (UniProtKB_input UniProtKB-from-symptoms/concept-ls)) 
   ((--involved_in--> '((rtx2 182 . "involved_in")))
    (--part_of--> '((rtx2 14 . "part_of")))
    (--subclass_of--> '((rtx2 15 . "subclass_of")))) 
   (UniProtKB_input --involved_in--> A --subclass_of--> B)))) 

(define UniProtKB_input--involved_in-->GO/concepts
  (hash-ref UniProtKB_input--involved_in-->GO--part_of-->GOparent=>concepts 'A))
(define UniProtKB_input--involved_in-->GO--part_of-->GOparent/concepts
  (hash-ref UniProtKB_input--involved_in-->GO--part_of-->GOparent=>concepts 'B)) 
(define UniProtKB_input--involved_in-->GO/edges
  (hash-ref UniProtKB_input--involved_in-->GO--part_of-->GOparent=>edges '--involved_in-->))
(define UniProtKB_input--involved_in-->GO--part_of-->GOparent/edges 
  (hash-ref UniProtKB_input--involved_in-->GO--part_of-->GOparent=>edges '--part_of-->)) 

;;rtx2 involved_in gives 21
(pretty-print (extract-name/curie/category-from-concept-ls UniProtKB_input--involved_in-->GO/concepts '()))  
 ;; part_of--> gives 25 
(pretty-print (extract-name/curie/category-from-concept-ls UniProtKB_input--involved_in-->GO--part_of-->GOparent/concepts '()))


(define UniProtKB_input--involved_in-->GO/edges/edges-for-print
  (edge-matcher UniProtKB_input--involved_in-->GO/edges '()))

(define UniProtKB_input--involved_in-->GO--part_of-->GOparent/edges-for-print
  (edge-matcher UniProtKB_input--involved_in-->GO--part_of-->GOparent/edges '()))

(define UniProtKB_input--involved_in-->GO/edges
  (hash-ref UniProtKB_input--involved_in-->GO--involved_in-->GOparent=>edges '--involved_in-->))

;;gives most relevant GO pathways
(define UniProtKB_input--involved_in-->GO--involved_in-->GOparent/edges
  (hash-ref UniProtKB_input--involved_in-->GO--involved_in-->GOparent=>edges '--involved_in-->))

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
            (list `((,subject-name . ,subject-id) ,pred (,object-name . ,object-id)) `(,db))
            els))])))))

(define UniProtKB_input--involved_in-->GO/edges-for-print
  (edge-matcher UniProtKB_input--involved_in-->GO/edges '()))

(define UniProtKB_input--involved_in-->GO--involved_in-->GOparent/edges-for-print
  (edge-matcher UniProtKB_input--involved_in-->GO--involved_in-->GOparent/edges '()))



















#|
(define encodes (find-exact-predicates (list "encodes")))
(define gene_associated_with_condition (find-exact-predicates (list "gene_associated_with_condition"))) 

(match-define
 (list hgnc--encodes-->B--gene_associated_with_condition-->D=>concepts
       B--contributes_to-->A--has_phenotype-->HP-ls=>edges)
 (time
  (run/graph
   ((B #f)
    (D #f)
    (hgnc gene-concept-ls))
   ((--encodes--> encodes)
    (--gene_associated_with_condition--> '((rtx2 717 . "gene_associated_with_condition"))))
   (hgnc --encodes--> B --gene_associated_with_condition--> D))))

(define hgnc--encodes-->B--gene_associated_with_condition-->D/concepts 
  (hash-ref hgnc--encodes-->B--gene_associated_with_condition-->D=>concepts 'D))

(filter-concept-by-f
   (find-concepts
    #t
    (list
     "HP:0001531"
     "HP:0003550"
     "HP:0001004"
     "HP:0008513"
     "HP:0002144"
     "HP:0001790")) '() 'orange member?)

(match-define
 (list B--contributes_to-->A--has_phenotype-->HP-ls=>concepts
       B--contributes_to-->A--has_phenotype-->HP-ls=>edges)
 (time
  (run/graph
   ((A #f)
    (B molecular_entity)
    (HP-ls HP-concept-ls))
   ((--has_phenotype--> has_phenotype)
    (--contributes_to--> contributes_to))
   (B --contributes_to--> A --has_phenotype--> HP-ls))))
|#

#|
;; need an exact predicate function 
(define (find-exact-predicates names)
(append* (map (lambda (name) (run* (x) (~predicateo name x))) names)))
(define (find-categories names)
(append* (map (lambda (name) (run* (x) (~categoryo name x))) names)))
(define (find-exact-categories names)
(run* (cat) (fresh (db catid name)
(membero name names)
(== cat `(,db ,catid . ,name))
(categoryo cat))))
(define (find-exact-predicates names)
(run* (cat) (fresh (db catid name)
(membero name names)
(== cat `(,db ,catid . ,name))
(categoryo cat))))
|#
          
;;TODO, load with only orange db to prevent has-phenotype from using other dbs 
#|
(match-define
 (list A-->HP-ls=>concepts
       A-->HP-ls=>edges)
 (time
  (run/graph
   ((A #f)
    (HP-ls HP-concept-ls))
   ((--has_phenotype--> has_phenotype)
    (--subclass_of--> subclass_of))
   (A --subclass_of--> HP-ls)
   (A --has_phenotype--> HP-ls))))

(define A-->has_phenotype-->HP-input/concepts
  (hash-ref A-->HP-ls=>concepts 'A))

(define A-->subclass_of-->HP-input/concepts
  (hash-ref A-->HP-ls=>concepts 'A))

(define A-->has_phenotype-->HP-input/edges
  (hash-ref A-->HP-ls=>edges '--has_phenotype-->))

(define A-->subclass_of-->HP-input/edges
  (hash-ref A-->HP-ls=>concepts 'subclass_of))

;; missing a DOID --has_phenotype--> HP in my orange KG
;; DOID concepts appear to have "disease" only names in Orange
;; that are clearly not disease in RTX2 robokop etc DOID:3457 example
;; perhaps filter by mondo concepts 
|#



























