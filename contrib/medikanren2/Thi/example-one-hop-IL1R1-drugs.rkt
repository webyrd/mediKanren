#lang racket/base
(require
  "query-low-level.rkt"
  racket/pretty)

(define drug-categories '("biolink:ChemicalSubstance"
                          "biolink:ClinicalIntervention"
                          "biolink:ClinicalModifier"
                          "biolink:Drug"
                          "biolink:Treatment"))

(define inhibit-preds '("biolink:decreases_activity_of"
                        "biolink:decreases_expression_of"
                        "biolink:disrupts"
                        "biolink:negatively_regulates"
                        "biolink:negatively_regulates,_entity_to_entity"
                        "biolink:negatively_regulates,_process_to_process"
                        "biolink:treats"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find drugs that inhibit IL1R1 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define IL1R1-synonyms '("NCBIGene:3554"
                         "PR:P14778"
                         "UniProtKB:P14778"
                         "UMLS:C1416395"
                         "HGNC:5993"
                         "ENSEMBL:ENSG00000115594"))

;; This use of query:X->Known is analogous to this miniKanren query:
;(run* (s sname p o oname)
;  (fresh (id drug)
;    (edge id s o)
;    (cprop s "category" drug)
;    (cprop s "name"  sname)
;    (cprop o "oname" oname)
;    (eprop id "predicate" p)
;    (membero o IL1R1-synonyms)
;    (membero p inhibit-preds)
;    (membero drug drug-categories)))))

(newline)
(define IL1R1-drugs (query:X->Known drug-categories inhibit-preds IL1R1-synonyms))

(pretty-write `(IL1R1-drugs count: ,(length IL1R1-drugs)))
(pretty-write IL1R1-drugs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find drugs that inhibit IL1R1 signalling pathway ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define IL1R1-pathway-synonyms '("ENSEMBL:ENSG00000172936"
                                 "ENSEMBL:LRG_157"
                                 "NCBIGene:4615"
                                 "HGNC:7562"
                                 "UniProtKB:Q99836"
                                 "PR:Q99836"
                                 "ENSEMBL:ENSG00000135341"
                                 "HGNC:6859"
                                 "NCBIGene:6885"
                                 "PR:O43318"
                                 "UniProtKB:O43318"
                                 "UniProtKB:Q9Y6K9"
                                 "ENSEMBL:ENSG00000269335"
                                 "NCBIGene:8517"
                                 "ENSEMBL:LRG_70"
                                 "HGNC:5961"
                                 "PR:Q9Y6K9"
                                 "HGNC:6112"
                                 "NCBIGene:3654"
                                 "UMLS:C1334134"
                                 "PR:P51617"
                                 "UniProtKB:P51617"
                                 "ENSEMBL:ENSG00000184216"
                                 "UniProtKB:O43187"
                                 "UMLS:C1334135"
                                 "HGNC:6113"
                                 "PR:O43187"
                                 "NCBIGene:3656"
                                 "ENSEMBL:ENSG00000134070"
                                 "HGNC:12036"
                                 "ENSEMBL:ENSG00000175104"
                                 "NCBIGene:7189"
                                 "UMLS:C1336666"
                                 "UniProtKB:Q9Y4K3"
                                 "PR:Q9Y4K3"
                                 "HGNC:6848"
                                 "PR:Q13233"
                                 "ENSEMBL:ENSG00000095015"
                                 "UniProtKB:Q13233"
                                 "NCBIGene:4214"
                                 "PR:P46734"
                                 "HGNC:6843"
                                 "NCBIGene:5606"
                                 "UniProtKB:P46734"
                                 "ENSEMBL:ENSG00000034152"
                                 "UMLS:C1456386"
                                 "ENSEMBL:ENSG00000108984"
                                 "UMLS:C1334475"
                                 "UniProtKB:P52564"
                                 "NCBIGene:5608"
                                 "HGNC:6846"
                                 "PR:P52564"
                                 "UniProtKB:P19838"
                                 "ENSEMBL:ENSG00000109320"
                                 "NCBIGene:4790"
                                 "HGNC:7794"
                                 "PR:P19838"
                                 "ENSEMBL:LRG_1316"
                                 "HGNC:6861"
                                 "NCBIGene:4293"
                                 "PR:P80192"
                                 "UMLS:C1417016"
                                 "UniProtKB:P80192"
                                 "ENSEMBL:ENSG00000006432"
                                 "NCBIGene:1432"
                                 "HGNC:6876"
                                 "ENSEMBL:ENSG00000112062"
                                 "UniProtKB:Q16539"
                                 "UMLS:C1366876"
                                 "PR:Q16539"
                                 "ENSEMBL:ENSG00000107643"
                                 "UMLS:C1367731"
                                 "PR:P45983"
                                 "HGNC:6881"
                                 "UniProtKB:P45983"
                                 "NCBIGene:5599"))

;; This use of query:X->Known is analogous to this miniKanren query:
;(run* (s sname p o oname)
;  (fresh (id drug)
;    (edge id s o)
;    (cprop s "category" drug)
;    (cprop s "name" sname)
;    (cprop o "name" oname)
;    (eprop id "predicate" p)
;    (membero o IL1R-pathway-synonyms)
;    (membero p inhibit-preds)
;    (membero drug drug-categories)))))

(newline)
(define IL1R1-pathway-drugs (query:X->Known drug-categories inhibit-preds IL1R1-pathway-synonyms))

(pretty-write `(IL1R1-pathway-drugs count: ,(length IL1R1-pathway-drugs)))
(pretty-write IL1R1-pathway-drugs)
