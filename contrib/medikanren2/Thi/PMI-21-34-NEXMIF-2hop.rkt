#lang racket

(require "../../../medikanren2/common.rkt")
(require "../../../medikanren2/db/rtx2-20210204.rkt")
(require "../../../medikanren2/lw-reasoning.rkt")
(require "../../../medikanren2/synonyms.rkt")

; Real use case PMI-21-34 for 2-hop query

; Find drugs/chemicals that inhibit certain gene/proteins that inhibit NEXMIF

; First, sanity check the curie for NEXMIF

(run* (k v)
  (cprop "HGNC:29433" k v))




; use the function get-synonyms-ls to get the list of synonyms for the curie
(get-synonyms-ls '("HGNC:29433"))

(define NEXMIF-syns '("HGNC:29433"
                      "UniProtKB:Q5QGS0"
                      "PR:Q5QGS0"
                      "NCBIGene:340533"
                      "ENSEMBL:ENSG00000050030"))


(define inhibit-preds '("biolink:decreases_activity_of"
                        "biolink:decreases_expression_of"
                        "biolink:disrupts"
                        "biolink:negatively_regulates"
                        "biolink:negatively_regulates,_entity_to_entity"
                        "biolink:negatively_regulates,_process_to_process"
                        "biolink:treats"
                        "negatively_regulates" ; semmed
                        "treats" ; semmed
                        ))

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


(define NEXMIF-drugs-2hop (time (run* (d p1 g1 p2 g2)
                                  (fresh (id1 id2 drug gene)
                                    (edge id1 d g1)
                                    (edge id2 g1 g2)
                                    (cprop d "category" drug)
                                    (cprop g1 "category" gene)
                                    (cprop g2 "category" gene)
                                    (eprop id1 "predicate" p1)
                                    (eprop id2 "predicate" p2)
                                    (membero drug drug-categories)
                                    (membero gene gene-protein-cats)
                                    (membero g2 NEXMIF-syns)
                                    (membero p1 inhibit-preds)
                                    (membero p2 inhibit-preds)))))

(define NEXMIF-drugs-2hop (time (run 1 (d p1 g1 p2 g2)
                                  (fresh (id1 id2 drug gene)
                                    (edge id1 d g1)
                                    (edge id2 g1 g2)
                                    (cprop d "category" drug)
                                    (cprop g1 "category" gene)
                                    (cprop g2 "category" gene)
                                    (eprop id1 "predicate" p1)
                                    (eprop id2 "predicate" p2)
                                    (membero drug drug-categories)
                                    (membero gene gene-protein-cats)
                                    (membero g2 NEXMIF-syns)
                                    (membero p1 inhibit-preds)
                                    (membero p2 inhibit-preds)))))
