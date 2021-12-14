#lang racket/base
(require
  "query-low-level.rkt"
  racket/pretty)

(define drug-categories '("biolink:ChemicalSubstance"
                          "biolink:ClinicalIntervention"
                          "biolink:ClinicalModifier"
                          "biolink:Drug"
                          "biolink:Treatment"))

(define gene-protein-cats '("biolink:Gene"
                            "biolink:GeneFamily"
                            "biolink:GeneProduct"
                            "biolink:GenomicEntity"
                            "biolink:MolecularEntity"
                            "biolink:Protein"))

(define inhibit-preds '("biolink:decreases_activity_of"
                        "biolink:decreases_expression_of"
                        "biolink:disrupts"
                        "biolink:negatively_regulates"
                        "biolink:negatively_regulates,_entity_to_entity"
                        "biolink:negatively_regulates,_process_to_process"
                        "biolink:treats"))

(define NEXMIF-syns '("HGNC:29433"
                      "UniProtKB:Q5QGS0"
                      "PR:Q5QGS0"
                      "NCBIGene:340533"
                      "ENSEMBL:ENSG00000050030"))

;; Reference query:
;(define NEXMIF-drugs-2hop
;  (time (run* (d p1 g1 p2 g2)
;          (fresh (id1 id2 drug gene)
;            (edge id1 d g1)
;            (edge id2 g1 g2)
;            (cprop d "category" drug)
;            (cprop g1 "category" gene)
;            (cprop g2 "category" gene)
;            (eprop id1 "predicate" p1)
;            (eprop id2 "predicate" p2)
;            (membero drug drug-categories)
;            (membero gene gene-protein-cats)
;            (membero g2 NEXMIF-syns)
;            (membero p1 inhibit-preds)
;            (membero p2 inhibit-preds)))))

(newline)
(define NEXMIF-drugs-2hop (query:X->Y->Known
                            drug-categories
                            inhibit-preds
                            gene-protein-cats
                            inhibit-preds
                            NEXMIF-syns))

(pretty-write `(NEXMIF-drugs-2hop count: ,(length NEXMIF-drugs-2hop)))
(pretty-write NEXMIF-drugs-2hop)
