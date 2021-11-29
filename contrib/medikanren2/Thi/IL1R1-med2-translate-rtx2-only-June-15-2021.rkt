#lang racket

(require "../../../medikanren2/common.rkt")
(require "../../../medikanren2/db/rtx2-20210204.rkt")
(require "../../../medikanren2/lw-reasoning.rkt")
(require "../../../medikanren2/synonyms.rkt")


; Date: June 15, 2021
; Goal: translate some  medikanren1 common queries into medikanren2



; Query 1 : find drugs that inhibits IL1R1


; first, sanity check the curie

(run* (k v)
  (cprop  "HGNC:5993" k v))

(define IL1R1 "HGNC:5993")

; find synonyms of this HGNC curie using the synonym relation defined in synonyms.rkt
; this approach uses the information in rtx2 to get synonyms. I prefer this for gene/protein.


;(run*/set/steps 1000 x (synonym x IL1R1))

; July 12th, working on the branch origin/prune-cyclic-calls
(time (run*/set x (synonym x IL1R1)))
; doens't terminate



;(run*/set x (kgx-synonym x IL1R1))
; results:
(set
 "NCBIGene:3554"
 "PR:P14778"
 "UniProtKB:P14778"
 "UMLS:C1416395"
 "HGNC:5993"
 "ENSEMBL:ENSG00000115594")

(define IL1R1-syns (list
                    "NCBIGene:3554"
                    "PR:P14778"
                    "UniProtKB:P14778"
                    "UMLS:C1416395"
                    "HGNC:5993"
                    "ENSEMBL:ENSG00000115594"))


#;(define inhibit-preds '("biolink:decreases_activity_of"
                        "biolink:decreases_expression_of"
                        "biolink:disrupts"
                        "biolink:negatively_regulates"
                        "biolink:negatively_regulates,_entity_to_entity"
                        "biolink:negatively_regulates,_process_to_process"
                        "biolink:treats"
                        "negatively_regulates" ; semmed
                        "treats" ; semmed
                        ))

; main query to find drugs that inhibit ILR1- synonyms

(define IL1R1-drugs (time (run* (s sname p o)
                            (fresh (id drug)
                              (edge id s o)
                              (cprop s "category" drug)
                              (cprop s "name" sname)
                              (eprop id "predicate" p)
                              (membero o IL1R1-syns)
                              (membero p inhibit-preds)
                              (membero drug drug-categories)))))


;cpu time: 10731 real time: 10909 gc time: 107

; export the results
(write-list-to-tsv
 (list "id" "drug-name" "pred" "gene")
 IL1R1-drugs
 "IL1R1-drugs.tsv")


;; Query 2: find drugs that inhibit IL1R1 signaling pathway


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

; find the synonyms for each of the IL1R-pathway genes
(time (map
       (lambda (x)
         (run*/set/steps 500 a
                         (synonym a x)))
       IL1R-pathway))

; point-free style way of writing the above code, i.e. no variables here, just apply functions on function's output

(time (set->list (apply set-union (map
                                   (lambda (x)
                                     (run*/set/steps 500 a
                                                     (synonym a x)))
                                   IL1R-pathway))))


; alternatively I can make the code easier to ready by using let to give names to function's output

(time (let* ((set-of-syn* (map
                           (lambda (x)
                             (run*/set/steps 500 a
                                             (synonym a x)))
                           IL1R-pathway))
             (set-of-syn (apply set-union set-of-syn*))
             (syn* (set->list set-of-syn)))
        syn*))

; from this, I can make a reusable function to take a list of HGNC curies genes and return the list of combined synonyms




(define IL1R1-pathway-syns (set->list (set-union
                                       (set
                                        "ENSEMBL:ENSG00000172936"
                                        "ENSEMBL:LRG_157"
                                        "NCBIGene:4615"
                                        "HGNC:7562"
                                        "UniProtKB:Q99836"
                                        "PR:Q99836")
                                       (set
                                        "ENSEMBL:ENSG00000135341"
                                        "HGNC:6859"
                                        "NCBIGene:6885"
                                        "PR:O43318"
                                        "UniProtKB:O43318")
                                       (set
                                        "UniProtKB:Q9Y6K9"
                                        "ENSEMBL:ENSG00000269335"
                                        "NCBIGene:8517"
                                        "ENSEMBL:LRG_70"
                                        "HGNC:5961"
                                        "PR:Q9Y6K9")
                                       (set
                                        "HGNC:6112"
                                        "NCBIGene:3654"
                                        "UMLS:C1334134"
                                        "PR:P51617"
                                        "UniProtKB:P51617"
                                        "ENSEMBL:ENSG00000184216")
                                       (set
                                        "UniProtKB:O43187"
                                        "UMLS:C1334135"
                                        "HGNC:6113"
                                        "PR:O43187"
                                        "NCBIGene:3656"
                                        "ENSEMBL:ENSG00000134070")
                                       (set
                                        "HGNC:12036"
                                        "ENSEMBL:ENSG00000175104"
                                        "NCBIGene:7189"
                                        "UMLS:C1336666"
                                        "UniProtKB:Q9Y4K3"
                                        "PR:Q9Y4K3")
                                       (set
                                        "HGNC:6848"
                                        "PR:Q13233"
                                        "ENSEMBL:ENSG00000095015"
                                        "UniProtKB:Q13233"
                                        "NCBIGene:4214")
                                       (set
                                        "PR:P46734"
                                        "HGNC:6843"
                                        "NCBIGene:5606"
                                        "UniProtKB:P46734"
                                        "ENSEMBL:ENSG00000034152"
                                        "UMLS:C1456386")
                                       (set
                                        "ENSEMBL:ENSG00000108984"
                                        "UMLS:C1334475"
                                        "UniProtKB:P52564"
                                        "NCBIGene:5608"
                                        "HGNC:6846"
                                        "PR:P52564")
                                       (set
                                        "UniProtKB:P19838"
                                        "ENSEMBL:ENSG00000109320"
                                        "NCBIGene:4790"
                                        "HGNC:7794"
                                        "PR:P19838"
                                        "ENSEMBL:LRG_1316")
                                       (set
                                        "HGNC:6861"
                                        "NCBIGene:4293"
                                        "PR:P80192"
                                        "UMLS:C1417016"
                                        "UniProtKB:P80192"
                                        "ENSEMBL:ENSG00000006432")
                                       (set
                                        "NCBIGene:1432"
                                        "HGNC:6876"
                                        "ENSEMBL:ENSG00000112062"
                                        "UniProtKB:Q16539"
                                        "UMLS:C1366876"
                                        "PR:Q16539")
                                       (set
                                        "ENSEMBL:ENSG00000107643"
                                        "UMLS:C1367731"
                                        "PR:P45983"
                                        "HGNC:6881"
                                        "UniProtKB:P45983"
                                        "NCBIGene:5599"))))

; 76 synonyms





(define IL1R1-pathway-drugs (time (run* (s sname p o oname)
                                    (fresh (id drug)
                                      (edge id s o)
                                      (cprop s "category" drug)
                                      (cprop s "name" sname)
                                      (cprop o "name" oname)
                                      (eprop id "predicate" p)
                                      (membero o IL1R1-pathway-syns)
                                      (membero p inhibit-preds)
                                      (membero drug drug-categories)))))

; run time to run through 76 synonymized IL1R pathway members


(write-list-to-tsv
 (list "drug-id" "drug-name" "pred" "gene-id" "gene-name")
  IL1R1-pathway-drugs
 "IL1R1-pathway-drugs_June15_2021.tsv")

; cpu time: 680250 real time: 825991 gc time: 5140
; ~15 mins
; once I have this table exported, I can use R to count duplicate drugs for different gene-id
; that way I can rank drugs that inhibit the most genes


; Next steps: we can test these queries in ARS 
