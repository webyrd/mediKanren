#lang racket

(require "../../../medikanren2/common.rkt")
(require "../../../medikanren2/db/rtx2-20210204.rkt")


; Goal: translate some  medikanren1 common queries into medikanren2
; note: just try rtx2 first

; Query: find drugs that inhibits IL1R1

#|
; first, sanity check the curie
(run* (k v)
    (cprop "HGNC:5993" k v))
|#

(define IL1R1 "HGNC:5993")

(define IL1R1-drugs (time (run 10 (s sname p o)
                            (fresh (id drug)
                              (edge id s o)
                              (cprop s "category" drug)
                              (cprop s "name" sname)
                              (eprop id "predicate" p)
                              (membero o IL1R1-synonyms)
                              (membero p inhibit-preds)
                              (membero drug drug-categories)))))

(write-list-to-tsv
 (list "id" "drug-name" "pred" "gene")
 IL1R1-drugs
 "IL1R1-drugs.tsv")


(define drug-categories '("biolink:ChemicalSubstance"
                          "biolink:ClinicalIntervention"
                          "biolink:ClinicalModifier"
                          "biolink:Drug"
                          "biolink:Treatment"))



;; look for all strings that key for eprop tables

(run* eprop_key
  (fresh (id value)
    (eprop id eprop_key value)))

#|
;; all keys in the eprop table
 '("negated"
  "object"
  "predicate"
  "predicate_label"
  "provided_by"
  "publications"
  "publications_info"
  "relation"
  "relation_label"
  "subject"
  "update_date")
|#


; define a lists of inhibit predicates (chem->decrease->genes)

; check all predicates in our data:
(run* pred_name
  (fresh (id)
    (eprop id "predicate" pred_name)))


; look for all cprop keys

(run* key
  (fresh (id value)
    (cprop id key value)))

#|
; all cprop keys

 '("category"
  "category_label"
  "creation_date"
  "deprecated"
  "description"
  "full_name"
  "id"
  "iri"
  "name"
  "provided_by"
  "publications"
  "replaced_by"
  "synonym"
  "update_date")
|#

; look for all cprop values

(run* value
  (fresh (id)
   (cprop id "category" value)))

#|
; all cprop values
 '("biolink:Activity"
  "biolink:Agent"
  "biolink:AnatomicalEntity"
  "biolink:Attribute"
  "biolink:BiologicalEntity"
  "biolink:BiologicalProcess"
  "biolink:BiologicalProcessOrActivity"
  "biolink:Carbohydrate"
  "biolink:Cell"
  "biolink:CellularComponent"
  "biolink:ChemicalSubstance"
  "biolink:ClinicalIntervention"
  "biolink:ClinicalModifier"
  "biolink:DataFile"
  "biolink:Device"
  "biolink:Disease"
  "biolink:DiseaseOrPhenotypicFeature"
  "biolink:Drug"
  "biolink:EnvironmentalFeature"
  "biolink:EnvironmentalProcess"
  "biolink:Exon"
  "biolink:ExposureEvent"
  "biolink:FrequencyValue"
  "biolink:Gene"
  "biolink:GeneFamily"
  "biolink:GeneProduct"
  "biolink:GenomicEntity"
  "biolink:GeographicLocation"
  "biolink:GrossAnatomicalStructure"
  "biolink:IndividualOrganism"
  "biolink:InformationContentEntity"
  "biolink:LifeStage"
  "biolink:MacromolecularComplex"
  "biolink:MaterialSample"
  "biolink:Metabolite"
  "biolink:MolecularActivity"
  "biolink:MolecularEntity"
  "biolink:NamedThing"
  "biolink:OntologyClass"
  "biolink:OrganismTaxon"
  "biolink:OrganismalEntity"
  "biolink:Pathway"
  "biolink:Phenomenon"
  "biolink:PhenotypicFeature"
  "biolink:PhysicalEntity"
  "biolink:PhysiologicalProcess"
  "biolink:PopulationOfIndividualOrganisms"
  "biolink:Procedure"
  "biolink:Protein"
  "biolink:Publication"
  "biolink:QuantityValue"
  "biolink:RelationshipType"
  "biolink:Treatment")
|#


#|
; all preds in rtx2  

'("biolink:actively_involved_in"
  "biolink:affects"
  "biolink:affects_activity_of"
  "biolink:affects_degradation_of"
  "biolink:affects_localization_of"
  "biolink:affects_transport_of"
  "biolink:biomarker_for"
  "biolink:capable_of"
  "biolink:causes"
  "biolink:chemically_similar_to"
  "biolink:close_match"
  "biolink:coexists_with"
  "biolink:colocalizes_with"
  "biolink:contraindicated_for"
  "biolink:contributes_to"
  "biolink:correlated_with"
  "biolink:decreases_activity_of"
  "biolink:decreases_expression_of"
  "biolink:derives_from"
  "biolink:derives_into"
  "biolink:develops_from"
  "biolink:directly_interacts_with"
  "biolink:disease_has_basis_in"
  "biolink:disrupts"
  "biolink:enables"
  "biolink:expressed_in"
  "biolink:expresses"
  "biolink:gene_associated_with_condition"
  "biolink:has_attribute"
  "biolink:has_attribute_type"
  "biolink:has_completed"
  "biolink:has_count"
  "biolink:has_decreased_amount"
  "biolink:has_gene_product"
  "biolink:has_increased_amount"
  "biolink:has_input"
  "biolink:has_molecular_consequence"
  "biolink:has_not_completed"
  "biolink:has_output"
  "biolink:has_part"
  "biolink:has_participant"
  "biolink:has_phenotype"
  "biolink:has_quantitative_value"
  "biolink:has_route"
  "biolink:has_unit"
  "biolink:homologous_to"
  "biolink:in_taxon"
  "biolink:increases_activity_of"
  "biolink:increases_degradation_of"
  "biolink:increases_expression_of"
  "biolink:increases_metabolic_processing_of"
  "biolink:increases_stability_of"
  "biolink:interacts_with"
  "biolink:is_sequence_variant_of"
  "biolink:lacks_part"
  "biolink:located_in"
  "biolink:location_of"
  "biolink:manifestation_of"
  "biolink:model_of"
  "biolink:molecularly_interacts_with"
  "biolink:negatively_regulates"
  "biolink:negatively_regulates,_entity_to_entity"
  "biolink:negatively_regulates,_process_to_process"
  "biolink:occurs_in"
  "biolink:overlaps"
  "biolink:part_of"
  "biolink:participates_in"
  "biolink:physically_interacts_with"
  "biolink:positively_regulates"
  "biolink:positively_regulates,_entity_to_entity"
  "biolink:positively_regulates,_process_to_process"
  "biolink:preceded_by"
  "biolink:precedes"
  "biolink:predisposes"
  "biolink:prevents"
  "biolink:produces"
  "biolink:quantifier_qualifier"
  "biolink:regulates"
  "biolink:regulates,_process_to_process"
  "biolink:related_to"
  "biolink:same_as"
  "biolink:subclass_of"
  "biolink:synonym"
  "biolink:temporally_related_to"
  "biolink:treats")
|#

(define inhibit-preds '("biolink:decreases_activity_of"
                        "biolink:decreases_expression_of"
                        "biolink:disrupts"
                        "biolink:negatively_regulates"
                        "biolink:negatively_regulates,_entity_to_entity"
                        "biolink:negatively_regulates,_process_to_process"
                        "biolink:treats"))

(define synonyms-preds '("biolink:same_as"
                         "biolink:close_match"
                         "biolink:has_gene_product"))

(define-relation (direct-synonym a b)
  (fresh (id sp)
    (edge id a b)
    (eprop id "predicate" sp)
    (membero sp synonyms-preds)))

(define-relation (direct-synonym+ a b)
  (conde ((direct-synonym a b))
         ((fresh (mid)
            (direct-synonym a mid)
            (direct-synonym+ mid b)))))

(define-relation (synonym a b)
  (conde ((direct-synonym a b))
         ((direct-synonym b a))
         ((fresh (mid)
            (direct-synonym a mid)
            (synonym mid b)))
         ((fresh (mid)
            (direct-synonym b mid)
            (synonym mid a)))))

(remove-duplicates (run*/steps 500 s (synonym "HGNC:5993" s)))

(define IL1R1-synonyms '("UMLS:C1416395"
                         "ENSEMBL:ENSG00000115594"
                         "NCBIGene:3554"
                         "PR:P14778"
                         "UniProtKB:P14778"
                         "NCBIGene:3554"))

(define-relation/table (IL1R1-synonym curie)
  'source-stream (run*/steps 500 (s) (synonym "HGNC:5993" s)))

(define-relation/table (inhibit-pred predicate)
  'source-stream (map list inhibit-preds))

(define-relation/table (drug-category category)
  'source-stream (map list drug-categories))

;; instead of using membero, we can use relation/table to decrease run times (with source-stream), but still need some more tweaks

(define IL1R1-drugs2 (time (run 10 (s sname p o)
                             (fresh (id drug)
                               (edge id s o)
                               (cprop s "category" drug)
                               (cprop s "name" sname)
                               (eprop id "predicate" p)
                               ;(IL1R1-synonym o)
                               (inhibit-pred p)
                               (drug-category drug)
                               (membero o IL1R1-synonyms)
                               ;(membero p inhibit-preds)
                               ;(membero drug drug-categories)
                              ))))
