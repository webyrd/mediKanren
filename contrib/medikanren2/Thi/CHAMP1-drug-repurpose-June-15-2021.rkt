#lang racket

(require "../../../medikanren2/common.rkt")
(require "../../../medikanren2/db/rtx2-20210204.rkt")
(require "../../../medikanren2/lw-reasoning.rkt")
(require "../../../medikanren2/synonyms.rkt")
(require "Hakon-DEG-list.rkt")
(require "Hakon-DEG-syns.rkt")


; Date: June 15, 2021
; Query: Find drugs that up or down-regulate a list of genes while treat the phenotypes

; The list of genes were stored in Hakon-DEG-list.rkt
; The list of synonymized genes were stored in Hakon-DEG-syns.rkt

; First, look for synonyms of these genes using the synonym relation

(time (map
       (lambda (x)
         (run*/set/steps 500 a
                         (synonym a x)))
       Hakon-CHAMP1-RNA-up))

; the Hakon-CHAMP1-RNA-up contains 132 genes
; time: 154062 real time: 177755 gc time: 1053
; ~ 3 min
; save the results in a .rkt file

(length  Hakon-CHAMP1-RNA-up-syns)
; 760 synonyms for RNA-up


(time (map
       (lambda (x)
         (run*/set/steps 500 a
                         (synonym a x)))
       Hakon-CHAMP1-RNA-down))

(length  Hakon-CHAMP1-RNA-down-syns)

; the Hakon-CHAMP1-RNA-down contains  genes
; cpu time: 388297 real time: 427985 gc time: 2749
; 8 mins
; 2036 synonyms for RNA-down





;; These are PMI-18-04 phenotypes that we want to look for drugs to treat these conditions

(define hypotonia "HP:0001252")
(define impaired-convergence "HP:0000619")
(define short-attention-span "HP:0007018")
(define speech-apraxia "HP:0011098")
(define autism "MONDO:0005260")
(define cerebral-palsy "HP:0100021")
(define constipation "HP:0002019")
(define fatigue "HP:0012378")
(define gas-reflux "HP:0002020")
(define ID "HP:0001249")
(define speech-delay "HP:0000750")


; get the phenotype synonyms using the cached and indexed kgx-syn KG

(define hypotonia-syn (set->list (run*/set x (kgx-synonym x hypotonia))))

(get-names-ls hypotonia-syn)
'(("UMLS:C4049521" "Generalised atony")
  ("UMLS:C0541791" "Atonia")
  ("UMLS:C0541791" "Atonia")
  ("UMLS:C0026827" "Poor muscle tone")
  ("UMLS:C0026827" "Muscle hypotonia")
  ("MEDDRA:10058909" "Muscle relaxant therapy")
  ("UMLS:C0857516" "Floppy")
  ("UMLS:C0857516" "Floppy")
  ("UMLS:C1142135" "Muscle relaxant therapy")
  ("HP:0001252" "Muscular hypotonia")
  ("UMLS:C0541792" "Skeletal muscle atony")
  ("UMLS:C0541792" "Skeletal muscle atony")
  ("UMLS:C0859331" "Abdominal flaccidity")
  ("UMLS:C0857388" "Non-depolarising relaxant")
  ("MEDDRA:10021118" "Hypotonia")
  ("UMLS:C0857388" "Non-depolarising relaxant")
  ("NCIT:C87070" "Hypotonia"))
; 17 names with synonyms but only 14 unique curies

; note that there are duplicates when we look for names
; 14 synonyms for hypotonia





#|
; could try to do query expansion in the future
(time (run* hypotonia-subclasses
        (fresh (x)
          (subclass-of* hypotonia-subclasses x)
          (membero x hypotonia-syn))))
|#





; build main query to find drugs that inhibit genes and treat one phenotype, eg., hypotonia

(define Hakon-drugs-hypotonia-RNA-up
  (time
   (run* (d dname drug->gene-pred g gname disease disease-name drug->disease-pred)
     (fresh (id1 id2 drug)
       (edge id1 d g)
       (edge id2 d disease)
       (cprop d "category" drug)
       (cprop d "name" dname)
       (cprop g "name" gname)
       (cprop disease "name" disease-name)
       (eprop id1 "predicate" drug->gene-pred)
       (eprop id2 "predicate" drug->disease-pred)
       (membero drug drug-categories)
       (membero g Hakon-CHAMP1-RNA-up-syns)
       (membero drug->gene-pred inhibit-preds)
       (membero disease hypotonia-syn)
       (membero drug->disease-pred inhibit-preds)))))

(define Hakon-drugs-hypotonia-RNA-up
  (time
   (run 1 (d dname drug->gene-pred g gname disease disease-name drug->disease-pred)
     (fresh (id1 id2 drug)
       (edge id1 d g)
       (edge id2 d disease)
       (cprop d "category" drug)
       (cprop d "name" dname)
       (cprop g "name" gname)
       (cprop disease "name" disease-name)
       (eprop id1 "predicate" drug->gene-pred)
       (eprop id2 "predicate" drug->disease-pred)
       (membero drug drug-categories)
       (membero g Hakon-CHAMP1-RNA-up-syns)
       (membero drug->gene-pred inhibit-preds)
       (membero disease hypotonia-syn)
       (membero drug->disease-pred inhibit-preds)))))

; this query terminates

; cpu time: 17156 real time: 24444 gc time: 226


; I tried this query without asking for drug/gene/disease names and it terminated.
; First, try with run 1
; According to Greg, if run 1 terminates, the query will eventually terminate so it
; is good to check run 1 first.
(define Hakon-drugs-hypotonia-RNA-up
  (time
   (run 1 (d g disease)
     (fresh (id1 id2 drug drug->gene-pred drug->disease-pred)
       (edge id1 d g)
       (edge id2 d disease)
       (cprop d "category" drug)
       (eprop id1 "predicate" drug->gene-pred)
       (eprop id2 "predicate" drug->disease-pred)
       (membero drug drug-categories)
       (membero g Hakon-CHAMP1-RNA-up-syns)
       (membero drug->gene-pred inhibit-preds)
       (membero disease hypotonia-syn)
       (membero drug->disease-pred inhibit-preds)))))

; cpu time: 9583 real time: 12312 gc time: 36
; '(("UMLS:C0013227" "UMLS:C0812307" "UMLS:C0026827"))



; If we don't look for names, this query looped though 14 disease curies and ~ 200 genes
; to find drugs that inhibit both. This terminated in ~ 8 mins.

(define Hakon-drugs-hypotonia-RNA-up
  (time
   (run* (d g disease)
     (fresh (id1 id2 drug drug->gene-pred drug->disease-pred)
       (edge id1 d g)
       (edge id2 d disease)
       (cprop d "category" drug)
       (eprop id1 "predicate" drug->gene-pred)
       (eprop id2 "predicate" drug->disease-pred)
       (membero drug drug-categories)
       (membero g Hakon-CHAMP1-RNA-up-syns)
       (membero drug->gene-pred inhibit-preds)
       (membero disease hypotonia-syn)
       (membero drug->disease-pred inhibit-preds)))))




; cpu time: 404819 real time: 445583 gc time: 2925
; Hakon-drugs-hypotonia-RNA-up
'(("UMLS:C0013227" "UMLS:C0812307" "UMLS:C0026827")
  ("UMLS:C0040616" "UMLS:C0812307" "UMLS:C0026827")
  ("UMLS:C0013227" "UMLS:C0812307" "UMLS:C0026827")
  ("UMLS:C0040616" "UMLS:C0812307" "UMLS:C0026827")
  ("UMLS:C0013227" "UMLS:C0812307" "UMLS:C0541791")
  ("UMLS:C0022614" "NCBIGene:4773" "UMLS:C0026827")
  ("UMLS:C0028128" "NCBIGene:7351" "UMLS:C0026827")
  ("UMLS:C0724441" "NCBIGene:7351" "UMLS:C0026827")
  ("UMLS:C0008286" "NCBIGene:2149" "UMLS:C0026827")
  ("UMLS:C0013227" "UMLS:C1366449" "UMLS:C0026827")
  ("UMLS:C0013227" "UMLS:C1366449" "UMLS:C0026827")
  ("UMLS:C0013227" "UMLS:C1366449" "UMLS:C0541791")
  ("UMLS:C0013227" "NCBIGene:26033" "UMLS:C0026827")
  ("UMLS:C0013227" "NCBIGene:26033" "UMLS:C0026827")
  ("UMLS:C0013227" "NCBIGene:26033" "UMLS:C0541791")
  ("UMLS:C0013227" "NCBIGene:1021" "UMLS:C0026827")
  ("UMLS:C0013227" "NCBIGene:1021" "UMLS:C0026827")
  ("UMLS:C0013227" "NCBIGene:1021" "UMLS:C0541791")
  ("UMLS:C0006644" "NCBIGene:5984" "UMLS:C0026827")
  ("UMLS:C0013227" "NCBIGene:29126" "UMLS:C0026827")
  ("UMLS:C0013227" "NCBIGene:29126" "UMLS:C0026827")
  ("UMLS:C0030054" "NCBIGene:29126" "UMLS:C0026827")
  ("UMLS:C0013227" "NCBIGene:29126" "UMLS:C0541791")
  ("UMLS:C0012010" "NCBIGene:3977" "UMLS:C0026827")
  ("UMLS:C0012010" "NCBIGene:3977" "UMLS:C0026827")
  ("UMLS:C0026549" "NCBIGene:3977" "UMLS:C0026827")
  ("UMLS:C0724441" "NCBIGene:3977" "UMLS:C0026827")
  ("UMLS:C0033554" "NCBIGene:5733" "UMLS:C0541791"))



; Jeff suggested trying to the relation/table in place of membero to see if it speeds up the code
(define (Hakon-drugs-hypotonia-RNA-up)
  (time
   (define-relation/table (rel-drug arg1)
     'source-stream (map list drug-categories))
   (define-relation/table (rel-Hakon arg1)
     'source-stream (map list Hakon-CHAMP1-RNA-up-syns))
   (define-relation/table (rel-drug-gene-inhibit arg1)
     'source-stream (map list  inhibit-preds))
   (define-relation/table (rel-hypotonia-syn arg1)
     'source-stream (map list hypotonia-syn))
   (define-relation/table (rel-drug-disease-inhibit arg1)
     'source-stream (map list inhibit-preds))
   (run* (d g disease)
     (fresh (id1 id2 drug drug->gene-pred drug->disease-pred)
       (edge id1 d g)
       (edge id2 d disease)
       (cprop d "category" drug)
       (eprop id1 "predicate" drug->gene-pred)
       (eprop id2 "predicate" drug->disease-pred)
       (rel-drug drug)           ;(membero drug drug-categories)
       (rel-Hakon g)             ;(membero g Hakon-CHAMP1-RNA-up-syns)
       (rel-drug-gene-inhibit drug->gene-pred) ;(membero drug->gene-pred inhibit-preds)
       (rel-hypotonia-syn disease)    ;(membero disease hypotonia-syn)
       (rel-drug-disease-inhibit  drug->disease-pred) ; (membero drug->disease-pred inhibit-preds)
       ))))


(define (Hakon-drugs-hypotonia-RNA-up)
  (time
   (define-relation/table (rel-drug arg1)
     'source-stream (map list drug-categories))
   (define-relation/table (rel-Hakon arg1)
     'source-stream (map list Hakon-CHAMP1-RNA-up-syns))
   (define-relation/table (rel-drug-gene-inhibit arg1)
     'source-stream (map list  inhibit-preds))
   (define-relation/table (rel-hypotonia-syn arg1)
     'source-stream (map list hypotonia-syn))
   (define-relation/table (rel-drug-disease-inhibit arg1)
     'source-stream (map list inhibit-preds))
   (run 1 (d g disease)
     (fresh (id1 id2 drug drug->gene-pred drug->disease-pred)
       (edge id1 d g)
       (edge id2 d disease)
       (cprop d "category" drug)
       (eprop id1 "predicate" drug->gene-pred)
       (eprop id2 "predicate" drug->disease-pred)
       (rel-drug drug)           ;(membero drug drug-categories)
       (rel-Hakon g)             ;(membero g Hakon-CHAMP1-RNA-up-syns)
       (rel-drug-gene-inhibit drug->gene-pred) ;(membero drug->gene-pred inhibit-preds)
       (rel-hypotonia-syn disease)    ;(membero disease hypotonia-syn)
       (rel-drug-disease-inhibit  drug->disease-pred) ; (membero drug->disease-pred inhibit-preds)
       ))))

; This approach doesn't speed up the code for some reason and it may creates infinite loop
; => bugs in mediKanren2 or?

; Greg mentioned it is a query planning issue

; Jeff will take a look at this, June 15th, 2021.






; Next time, try the map in place of membero to see if it helps














; Here are a few work-around approaches that Greg wrote back in May 2021

(define-relation/table (inhibit-pred predicate)
  'source-stream (map list inhibit-preds))

(define Hakon-drugs-hypotonia-RNA-up
  (time
   (run 1 (d dname drug->gene-pred g gname disease drug->disease-pred disease-name)
     (fresh (id1 id2 drug)
       (edge id1 d g)
       (edge id2 d disease)
       (cprop d "category" drug)
       (eprop id1 "predicate" drug->gene-pred)
       (eprop id2 "predicate" drug->disease-pred)
       (inhibit-pred drug->gene-pred)
       (inhibit-pred drug->disease-pred)
       (membero g Hakon-CHAMP1-RNA-up-syn)
       (membero drug drug-categories)
       (membero disease hypotonia-syn)
       (:== dname (d) (car (run 1 name (cprop d "name" name))))
       (:== gname (g) (car (run 1 name (cprop g "name" name))))
       (:== disease-name (disease) (car (run 1 name (cprop disease "name" name))))))))

; cpu time: 34497 real time: 42562 gc time: 197

(define Hakon-drugs-hypotonia-RNA-up
  (time
   (run 1 (d dname drug->gene-pred g gname disease drug->disease-pred disease-name)
     (fresh (id1 id2 drug)
       (edge id1 d g)
       (edge id2 d disease)
       (cprop d "category" drug)
       (eprop id1 "predicate" drug->gene-pred)
       (eprop id2 "predicate" drug->disease-pred)
       (inhibit-pred drug->gene-pred)
       (inhibit-pred drug->disease-pred)
       (membero g Hakon-CHAMP1-RNA-up-syn)
       (membero drug drug-categories)
       (membero disease hypotonia-syn)
       (:== dname (d g disease) (car (run 1 name (cprop d "name" name))))
       (:== gname (d g disease) (car (run 1 name (cprop g "name" name))))
       (:== disease-name (d g disease) (car (run 1 name (cprop disease "name" name))))))))

;cpu time: 27237 real time: 27446 gc time: 179

(define-relation/table (drug-category category)
  'source-stream (map list drug-categories))


(define Hakon-drugs-hypotonia-RNA-up
  (time
   (run 1 (d dname drug->gene-pred g gname disease drug->disease-pred disease-name)
     (fresh (id1 id2 drug)
       (cprop d "category" drug)
       (drug-category drug)
       (edge id1 d g)
       (edge id2 d disease)
       (eprop id1 "predicate" drug->gene-pred)
       (eprop id2 "predicate" drug->disease-pred)
       (inhibit-pred drug->gene-pred)
       (inhibit-pred drug->disease-pred)
       (membero g Hakon-CHAMP1-RNA-up-syn)
       (membero disease hypotonia-syn)
       (:== dname (d g disease) (car (run 1 name (cprop d "name" name))))
       (:== gname (d g disease) (car (run 1 name (cprop g "name" name))))
       (:== disease-name (d g disease) (car (run 1 name (cprop disease "name" name))))
))))


; cpu time: 34839 real time: 35367 gc time: 196




(define Hakon-drugs-RNA-up
  (time
   (run* (d drug->gene-pred g)
     (fresh (id drug-cat)
       (cprop d "category" drug-cat)
       (drug-category drug-cat)
       (edge id d g)
       (eprop id "predicate" drug->gene-pred)
       (inhibit-pred drug->gene-pred)
       (membero g Hakon-CHAMP1-RNA-up-syn)))))

;cpu time: 404819 real time: 445583 gc time: 2925





;cpu time: 88185 real time: 100392 gc time: 579

(write-list-to-tsv
 (list "drug" "pred" "gene")
 Hakon-drugs-RNA-up
 "Hakon-drugs-RNA-up.tsv")

(define Hakon-drugs-disease
  (time
   (run* (d drug->disease-pred disease)
     (fresh (id drug-cat)
       (cprop d "category" drug-cat)
       (drug-category drug-cat)
       (edge id d disease)
       (eprop id "predicate" drug->disease-pred)
       (inhibit-pred drug->disease-pred)
       (membero disease hypotonia-syn)))))


(write-list-to-tsv
 (list "drug" "pred" "disease")
 Hakon-drugs-disease
 "Hakon-drugs-hypononia.tsv")


(define-relation/table (Hakon-drug curie)
  'source-stream
    (run^ (d)
      (fresh (id drug-cat drug->disease-pred disease)
       (cprop d "category" drug-cat)
       (drug-category drug-cat)
       (edge id d disease)
       (eprop id "predicate" drug->disease-pred)
       (inhibit-pred drug->disease-pred)
       (membero disease hypotonia-syn))))


