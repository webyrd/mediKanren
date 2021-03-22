#lang racket/base
(require "../common.rkt" "../db/rtx2-20210204.rkt" racket/pretty)

(define-syntax-rule (test name e expected)
  (begin (printf "Testing ~s:\n" name)
         (let ((answer (time e)))
           (unless (equal? answer expected)
             (pretty-write 'e)
             (printf "FAILED ~s:\n" name)
             (printf "  ANSWER:\n")
             (pretty-write answer)
             (printf "  EXPECTED:\n")
             (pretty-write expected)))))

(test 'various-concept-properties
  (run 5 (id k v) (cprop id k v))
  '(("MEDDRA:10001490" "category" "biolink:Activity")
    ("MEDDRA:10001494" "category" "biolink:Activity")
    ("MEDDRA:10001535" "category" "biolink:Activity")
    ("MEDDRA:10001846" "category" "biolink:Activity")
    ("MEDDRA:10003536" "category" "biolink:Activity")))

(test 'various-edges
  (run 5 (id s o) (edge id s o))
  '((12605868 "EHDAA2:0004546" "AEO:0000013")
    (12606986 "EHDAA2:0000152" "AEO:0000078")
    (12606988 "EHDAA2:0000503" "AEO:0000078")
    (12606984 "EHDAA2:0000935" "AEO:0000078")
    (12606994 "EHDAA2:0001051" "AEO:0000078")))

(test 'various-edge-properties
  (run 5 (id k v) (eprop id k v))
  '((0 "negated" "False")
    (1 "negated" "False")
    (2 "negated" "False")
    (3 "negated" "False")
    (4 "negated" "False")))

(test 'all-concept-properties
  (run* k
    (fresh (curie v)
      (cprop curie k v)))
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
    "update_date"))

(test 'all-edge-properties
  (run* k
    (fresh (eid v)
      (eprop eid k v)))
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
    "update_date"))

(test 'all-concept-categories
  (run* category
    (fresh (curie)
      (cprop curie "category" category)))
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
    "biolink:Treatment"))

(test 'all-predicates
  (run* predicate
    (fresh (eid)
      (eprop eid "predicate" predicate)))
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
    "biolink:treats"))

(test 'all-relations
  (run* relation
    (fresh (eid)
      (eprop eid "relation" relation)))
  '("BFO:0000050"
    "BFO:0000051"
    "BFO:0000054"
    "BFO:0000055"
    "BFO:0000056"
    "BFO:0000062"
    "BFO:0000063"
    "BFO:0000066"
    "BFO:0000067"
    "BFO:0000068"
    "BFO:0000069"
    "BFO:0000117"
    "BFO:0000167"
    "BSPO:0000096"
    "BSPO:0000097"
    "BSPO:0000098"
    "BSPO:0000099"
    "BSPO:0000100"
    "BSPO:0000102"
    "BSPO:0000104"
    "BSPO:0000107"
    "BSPO:0000108"
    "BSPO:0000110"
    "BSPO:0000113"
    "BSPO:0000120"
    "BSPO:0000121"
    "BSPO:0000122"
    "BSPO:0000123"
    "BSPO:0000124"
    "BSPO:0000125"
    "BSPO:0000126"
    "BSPO:0001100"
    "BSPO:0001101"
    "BSPO:0001106"
    "BSPO:0001107"
    "BSPO:0001108"
    "BSPO:0001113"
    "BSPO:0001115"
    "BSPO:0005001"
    "BSPO:0015001"
    "BSPO:0015002"
    "BSPO:0015003"
    "BSPO:0015005"
    "BSPO:0015006"
    "BSPO:0015007"
    "BSPO:0015008"
    "BSPO:0015009"
    "BSPO:0015012"
    "BSPO:0015014"
    "BSPO:0015101"
    "BSPO:0015102"
    "BSPO:0015202"
    "BTO:develops_from"
    "BTO:related_to"
    "CHEBI:has_functional_parent"
    "CHEBI:has_parent_hydride"
    "CHEBI:is_conjugate_acid_of"
    "CHEBI:is_conjugate_base_of"
    "CHEBI:is_enantiomer_of"
    "CHEBI:is_substituent_group_from"
    "CHEBI:is_tautomer_of"
    "CHEMBL.MECHANISM:activator"
    "CHEMBL.MECHANISM:agonist"
    "CHEMBL.MECHANISM:allosteric_antagonist"
    "CHEMBL.MECHANISM:antagonist"
    "CHEMBL.MECHANISM:antisense_inhibitor"
    "CHEMBL.MECHANISM:binding_agent"
    "CHEMBL.MECHANISM:blocker"
    "CHEMBL.MECHANISM:chelating_agent"
    "CHEMBL.MECHANISM:cross-linking_agent"
    "CHEMBL.MECHANISM:degrader"
    "CHEMBL.MECHANISM:disrupting_agent"
    "CHEMBL.MECHANISM:equivalent_to"
    "CHEMBL.MECHANISM:hydrolytic_enzyme"
    "CHEMBL.MECHANISM:inhibitor"
    "CHEMBL.MECHANISM:inverse_agonist"
    "CHEMBL.MECHANISM:modulator"
    "CHEMBL.MECHANISM:negative_allosteric_modulator"
    "CHEMBL.MECHANISM:negative_modulator"
    "CHEMBL.MECHANISM:opener"
    "CHEMBL.MECHANISM:overlaps_with"
    "CHEMBL.MECHANISM:oxidative_enzyme"
    "CHEMBL.MECHANISM:partial_agonist"
    "CHEMBL.MECHANISM:positive_allosteric_modulator"
    "CHEMBL.MECHANISM:positive_modulator"
    "CHEMBL.MECHANISM:proteolytic_enzyme"
    "CHEMBL.MECHANISM:reducing_agent"
    "CHEMBL.MECHANISM:releasing_agent"
    "CHEMBL.MECHANISM:sequestering_agent"
    "CHEMBL.MECHANISM:stabiliser"
    "CHEMBL.MECHANISM:subset_of"
    "CHEMBL.MECHANISM:substrate"
    "CHEMBL.MECHANISM:superset_of"
    "CL:has_completed"
    "CL:has_high_plasma_membrane_amount"
    "CL:has_low_plasma_membrane_amount"
    "CL:has_not_completed"
    "CL:lacks_part"
    "CL:lacks_plasma_membrane_part"
    "CPT:has_add_on_code"
    "CPT:has_possibly_included_panel_element"
    "CPT:mapped_to"
    "CPT:panel_element_of"
    "CPT:panel_element_of_possibly_included"
    "DDANAT:develops_from"
    "DGIdb:activator"
    "DGIdb:adduct"
    "DGIdb:affects"
    "DGIdb:agonist"
    "DGIdb:allosteric_modulator"
    "DGIdb:antagonist"
    "DGIdb:antibody"
    "DGIdb:antisense_oligonucleotide"
    "DGIdb:binder"
    "DGIdb:blocker"
    "DGIdb:chaperone"
    "DGIdb:cleavage"
    "DGIdb:cofactor"
    "DGIdb:inducer"
    "DGIdb:inhibitor"
    "DGIdb:inhibitory_allosteric_modulator"
    "DGIdb:inverse_agonist"
    "DGIdb:ligand"
    "DGIdb:modulator"
    "DGIdb:multitarget"
    "DGIdb:negative_modulator"
    "DGIdb:partial_agonist"
    "DGIdb:partial_antagonist"
    "DGIdb:positive_modulator"
    "DGIdb:potentiator"
    "DGIdb:product_of"
    "DGIdb:stimulator"
    "DGIdb:substrate"
    "DGIdb:suppressor"
    "DGIdb:vaccine"
    "DRUGBANK:category"
    "DRUGBANK:drug-interaction"
    "DRUGBANK:external-identifier"
    "DRUGBANK:pathway"
    "DRUGBANK:target"
    "EFO:0000784"
    "EFO:0001697"
    "EFO:0006351"
    "EFO:is_executed_in"
    "ENVO:01001307"
    "FMA:adheres_to"
    "FMA:adjacent_to"
    "FMA:afferent_to"
    "FMA:anterior_to"
    "FMA:anteroinferior_to"
    "FMA:anterolateral_to"
    "FMA:anteromedial_to"
    "FMA:anterosuperior_to"
    "FMA:arterial_supply_of"
    "FMA:articulates_with"
    "FMA:attaches_to"
    "FMA:bounded_by"
    "FMA:bounds"
    "FMA:branch_of"
    "FMA:connected_to"
    "FMA:connection_type_of"
    "FMA:constitutional_part_of"
    "FMA:contained_in"
    "FMA:contains"
    "FMA:continuation_branch_of"
    "FMA:continuous_distally_with"
    "FMA:continuous_proximally_with"
    "FMA:continuous_with"
    "FMA:corresponds_to"
    "FMA:derives"
    "FMA:derives_from"
    "FMA:development_type_of"
    "FMA:developmental_stage_of"
    "FMA:develops_from"
    "FMA:develops_into"
    "FMA:direct_cell_shape_of"
    "FMA:direct_left_of"
    "FMA:direct_right_of"
    "FMA:distal_to"
    "FMA:drains_into"
    "FMA:efferent_to"
    "FMA:external_to"
    "FMA:formed_by"
    "FMA:forms"
    "FMA:full_grown_phenotype_of"
    "FMA:fuses_with"
    "FMA:fusion_of"
    "FMA:germ_origin_of"
    "FMA:has_adherent"
    "FMA:has_arterial_supply"
    "FMA:has_branch"
    "FMA:has_connection_type"
    "FMA:has_constitutional_part"
    "FMA:has_continuation_branch"
    "FMA:has_development_type"
    "FMA:has_developmental_stage"
    "FMA:has_direct_cell_shape"
    "FMA:has_full_grown_phenotype"
    "FMA:has_fusion"
    "FMA:has_germ_origin"
    "FMA:has_inherent_3d_shape"
    "FMA:has_insertion"
    "FMA:has_location"
    "FMA:has_lymphatic_drainage"
    "FMA:has_member"
    "FMA:has_nerve_supply"
    "FMA:has_observed_anatomical_entity"
    "FMA:has_origin"
    "FMA:has_part"
    "FMA:has_primary_segmental_supply"
    "FMA:has_projection"
    "FMA:has_regional_part"
    "FMA:has_related_developmental_entity"
    "FMA:has_secondary_segmental_supply"
    "FMA:has_segmental_composition"
    "FMA:has_segmental_supply"
    "FMA:has_tributary"
    "FMA:has_venous_drainage"
    "FMA:homonym_of"
    "FMA:inferior_to"
    "FMA:inferolateral_to"
    "FMA:inferomedial_to"
    "FMA:inherent_3d_shape_of"
    "FMA:insertion_of"
    "FMA:internal_to"
    "FMA:lateral_to"
    "FMA:left_lateral_to"
    "FMA:left_medial_to"
    "FMA:location_of"
    "FMA:lymphatic_drainage_of"
    "FMA:matures_from"
    "FMA:matures_into"
    "FMA:medial_to"
    "FMA:member_of"
    "FMA:merges_with"
    "FMA:nerve_supply_of"
    "FMA:origin_of"
    "FMA:part_of"
    "FMA:posterior_to"
    "FMA:posteroinferior_to"
    "FMA:posterolateral_to"
    "FMA:posteromedial_to"
    "FMA:posterosuperior_to"
    "FMA:primary_segmental_supply_of"
    "FMA:projects_from"
    "FMA:projects_to"
    "FMA:proximal_to"
    "FMA:receives_attachment_from"
    "FMA:receives_drainage_from"
    "FMA:receives_input_from"
    "FMA:receives_projection"
    "FMA:regional_part_of"
    "FMA:related_developmental_entity_of"
    "FMA:related_object"
    "FMA:related_part"
    "FMA:right_lateral_to"
    "FMA:right_medial_to"
    "FMA:secondary_segmental_supply_of"
    "FMA:segmental_composition_of"
    "FMA:segmental_supply_of"
    "FMA:sends_output_to"
    "FMA:superior_to"
    "FMA:superolateral_to"
    "FMA:superomedial_to"
    "FMA:surrounded_by"
    "FMA:surrounds"
    "FMA:transforms_from"
    "FMA:transforms_into"
    "FMA:tributary_of"
    "FMA:venous_drainage_of"
    "FOODON:00001301"
    "FOODON:00001563"
    "FOODON:00002420"
    "GENEPIO:0001739"
    "GO:acts_upstream_of"
    "GO:acts_upstream_of_negative_effect"
    "GO:acts_upstream_of_or_within"
    "GO:acts_upstream_of_or_within_negative_effect"
    "GO:acts_upstream_of_or_within_positive_effect"
    "GO:acts_upstream_of_positive_effect"
    "GO:colocalizes_with"
    "GO:contributes_to"
    "GO:enables"
    "GO:ends_during"
    "GO:happens_during"
    "GO:has_occurrence"
    "GO:has_part"
    "GO:inverse_ends_during"
    "GO:inverse_isa"
    "GO:involved_in"
    "GO:is_active_in"
    "GO:isa"
    "GO:negatively_regulated_by"
    "GO:negatively_regulates"
    "GO:occurs_in"
    "GO:part_of"
    "GO:positively_regulated_by"
    "GO:positively_regulates"
    "GO:regulated_by"
    "GO:regulates"
    "GOREL:0000040"
    "GOREL:0002003"
    "GOREL:0002004"
    "GOREL:0002005"
    "GOREL:0012006"
    "HANCESTRO:0301"
    "HANCESTRO:0330"
    "HCPCS:mapped_to"
    "HMDB:at_cellular_location"
    "HMDB:at_tissue"
    "HMDB:disease"
    "HMDB:has_protein_association"
    "HMDB:in_biospecimen"
    "HMDB:in_pathway"
    "IAO:0000039"
    "IAO:0000136"
    "IAO:0000219"
    "IDO:0000664"
    "LOINC:analyzes"
    "LOINC:associated_with"
    "LOINC:class_of"
    "LOINC:component_of"
    "LOINC:has_action_guidance"
    "LOINC:has_adjustment"
    "LOINC:has_aggregation_view"
    "LOINC:has_answer"
    "LOINC:has_approach_guidance"
    "LOINC:has_archetype"
    "LOINC:has_challenge"
    "LOINC:has_class"
    "LOINC:has_component"
    "LOINC:has_count"
    "LOINC:has_divisor"
    "LOINC:has_evaluation"
    "LOINC:has_exam"
    "LOINC:has_fragments_for_synonyms"
    "LOINC:has_imaged_location"
    "LOINC:has_imaging_focus"
    "LOINC:has_lateral_anatomic_location"
    "LOINC:has_lateral_location_presence"
    "LOINC:has_member"
    "LOINC:has_method"
    "LOINC:has_modality_subtype"
    "LOINC:has_modality_type"
    "LOINC:has_object_guidance"
    "LOINC:has_parent_group"
    "LOINC:has_pharmaceutical_route"
    "LOINC:has_scale"
    "LOINC:has_subject"
    "LOINC:has_suffix"
    "LOINC:has_supersystem"
    "LOINC:has_system"
    "LOINC:has_time_aspect"
    "LOINC:has_time_modifier"
    "LOINC:has_timing_of"
    "LOINC:has_view_type"
    "LOINC:is_given_pharmaceutical_substance_for"
    "LOINC:is_presence_guidance_for"
    "LOINC:mapped_to"
    "LOINC:measured_by"
    "LOINC:member_of"
    "LOINC:mth_has_expanded_form"
    "LOINC:multipart_of"
    "LOINC:property_of"
    "MEDDRA:classified_as"
    "MEDDRA:has_member"
    "MEDDRA:member_of"
    "MESH:RO"
    "MESH:has_mapping_qualifier"
    "MESH:inverse_isa"
    "MESH:isa"
    "MESH:mapped_to"
    "MONDO:disease_causes_feature"
    "MONDO:disease_has_basis_in_accumulation_of"
    "MONDO:disease_has_basis_in_development_of"
    "MONDO:disease_has_major_feature"
    "MONDO:disease_responds_to"
    "MONDO:disease_shares_features_of"
    "MONDO:disease_triggers"
    "MONDO:equivalentTo"
    "MONDO:has_onset"
    "MONDO:part_of_progression_of_disease"
    "MONDO:predisposes_towards"
    "NCIT:allele_absent_from_wild-type_chromosomal_location"
    "NCIT:allele_has_abnormality"
    "NCIT:allele_has_activity"
    "NCIT:allele_in_chromosomal_location"
    "NCIT:allele_plays_altered_role_in_process"
    "NCIT:allele_plays_role_in_metabolism_of_chemical_or_drug"
    "NCIT:anatomic_structure_is_physical_part_of"
    "NCIT:biological_process_has_associated_location"
    "NCIT:biological_process_has_result_anatomy"
    "NCIT:biological_process_has_result_biological_process"
    "NCIT:biological_process_has_result_chemical_or_drug"
    "NCIT:biological_process_involves_chemical_or_drug"
    "NCIT:cdrh_parent_of"
    "NCIT:chemical_or_drug_affects_abnormal_cell"
    "NCIT:chemical_or_drug_affects_cell_type_or_tissue"
    "NCIT:chemical_or_drug_affects_gene_product"
    "NCIT:chemical_or_drug_has_mechanism_of_action"
    "NCIT:chemical_or_drug_has_physiologic_effect"
    "NCIT:chemical_or_drug_initiates_biological_process"
    "NCIT:chromosome_mapped_to_disease"
    "NCIT:complex_has_physical_part"
    "NCIT:conceptual_part_of"
    "NCIT:ctcae_5_parent_of"
    "NCIT:cytogenetic_abnormality_involves_chromosome"
    "NCIT:disease_has_associated_anatomic_site"
    "NCIT:disease_has_finding"
    "NCIT:disease_has_metastatic_anatomic_site"
    "NCIT:disease_has_molecular_abnormality"
    "NCIT:disease_has_normal_tissue_origin"
    "NCIT:disease_has_primary_anatomic_site"
    "NCIT:disease_is_grade"
    "NCIT:disease_is_marked_by_gene"
    "NCIT:disease_is_stage"
    "NCIT:disease_may_have_abnormal_cell"
    "NCIT:disease_may_have_associated_disease"
    "NCIT:disease_may_have_finding"
    "NCIT:disease_may_have_molecular_abnormality"
    "NCIT:enzyme_metabolizes_chemical_or_drug"
    "NCIT:eo_disease_has_associated_cell_type"
    "NCIT:eo_disease_has_associated_eo_anatomy"
    "NCIT:eo_disease_has_property_or_attribute"
    "NCIT:gene_associated_with_disease"
    "NCIT:gene_encodes_gene_product"
    "NCIT:gene_found_in_organism"
    "NCIT:gene_has_physical_location"
    "NCIT:gene_in_chromosomal_location"
    "NCIT:gene_involved_in_molecular_abnormality"
    "NCIT:gene_involved_in_pathogenesis_of_disease"
    "NCIT:gene_is_biomarker_of"
    "NCIT:gene_is_biomarker_type"
    "NCIT:gene_is_element_in_pathway"
    "NCIT:gene_mapped_to_disease"
    "NCIT:gene_mutant_encodes_gene_product_sequence_variation"
    "NCIT:gene_plays_role_in_process"
    "NCIT:gene_product_expressed_in_tissue"
    "NCIT:gene_product_has_abnormality"
    "NCIT:gene_product_has_associated_anatomy"
    "NCIT:gene_product_has_biochemical_function"
    "NCIT:gene_product_has_chemical_classification"
    "NCIT:gene_product_has_organism_source"
    "NCIT:gene_product_has_structural_domain_or_motif"
    "NCIT:gene_product_is_biomarker_of"
    "NCIT:gene_product_is_biomarker_type"
    "NCIT:gene_product_is_element_in_pathway"
    "NCIT:gene_product_is_physical_part_of"
    "NCIT:gene_product_malfunction_associated_with_disease"
    "NCIT:gene_product_plays_role_in_biological_process"
    "NCIT:gene_product_sequence_variation_encoded_by_gene_mutant"
    "NCIT:gene_product_variant_of_gene_product"
    "NCIT:genetic_biomarker_related_to"
    "NCIT:has_data_element"
    "NCIT:has_inc_parent"
    "NCIT:has_nichd_parent"
    "NCIT:has_pharmaceutical_administration_method"
    "NCIT:has_pharmaceutical_basic_dose_form"
    "NCIT:has_pharmaceutical_intended_site"
    "NCIT:has_pharmaceutical_release_characteristics"
    "NCIT:has_pharmaceutical_state_of_matter"
    "NCIT:has_pharmaceutical_transformation"
    "NCIT:has_salt_form"
    "NCIT:has_target"
    "NCIT:human_disease_maps_to_eo_disease"
    "NCIT:is_abnormal_cell_of_disease"
    "NCIT:is_associated_disease_of"
    "NCIT:is_component_of_chemotherapy_regimen"
    "NCIT:is_cytogenetic_abnormality_of_disease"
    "NCIT:is_location_of_anatomic_structure"
    "NCIT:is_molecular_abnormality_of_disease"
    "NCIT:is_normal_cell_origin_of_disease"
    "NCIT:is_organism_source_of_gene_product"
    "NCIT:is_qualified_by"
    "NCIT:is_related_to_endogenous_product"
    "NCIT:may_be_normal_cell_origin_of_disease"
    "NCIT:neoplasm_has_special_category"
    "NCIT:organism_has_gene"
    "NCIT:pathway_has_gene_element"
    "NCIT:procedure_has_completely_excised_anatomy"
    "NCIT:procedure_has_excised_anatomy"
    "NCIT:procedure_has_imaged_anatomy"
    "NCIT:procedure_has_partially_excised_anatomy"
    "NCIT:procedure_has_target_anatomy"
    "NCIT:procedure_may_have_completely_excised_anatomy"
    "NCIT:procedure_may_have_excised_anatomy"
    "NCIT:procedure_may_have_partially_excised_anatomy"
    "NCIT:process_includes_biological_process"
    "NCIT:process_initiates_biological_process"
    "NCIT:process_involves_gene"
    "NCIT:qualifier_applies_to"
    "NCIT:regimen_has_accepted_use_for_disease"
    "NCIT:role_has_domain"
    "NCIT:role_has_parent"
    "NCIT:role_has_range"
    "NCIT:subset_includes_concept"
    "NDDF:has_dose_form"
    "NDDF:has_ingredient"
    "NDDF:ingredient_of"
    "OBI:0000293"
    "OBI:0000295"
    "OBI:0000299"
    "OBI:0001927"
    "OBO:INO_0000154"
    "OBO:core#connected_to"
    "OBO:core#distally_connected_to"
    "OBO:core#innervated_by"
    "OBO:core#subdivision_of"
    "OBO:doid#derives_from"
    "OBO:doid#has_symptom"
    "OBO:envo#has_increased_levels_of"
    "OBO:exo.obo#interacts_with"
    "OBO:exo.obo#interacts_with_an_exposure_receptor_via"
    "OBO:exo.obo#interacts_with_an_exposure_stressor_via"
    "OBO:hancestro_0308"
    "OBO:has_role"
    "OBO:intersection_of"
    "OBO:nbo#by_means"
    "OBO:nbo#has_participant"
    "OBO:nbo#in_response_to"
    "OBO:nbo#is_about"
    "OBO:uo#is_unit_of"
    "OBO:xref"
    "OBOREL:bearer_of"
    "OMIM:allelic_variant_of"
    "OMIM:has_inheritance_type"
    "OMIM:has_manifestation"
    "OMIM:has_phenotype"
    "OMIM:manifestation_of"
    "OMIM:phenotype_of"
    "ORPHANET:317343"
    "ORPHANET:317344"
    "ORPHANET:317345"
    "ORPHANET:317346"
    "ORPHANET:317348"
    "ORPHANET:317349"
    "ORPHANET:327767"
    "ORPHANET:410295"
    "ORPHANET:410296"
    "ORPHANET:465410"
    "ORPHANET:C016"
    "ORPHANET:C017"
    "PATO:correlates_with"
    "PATO:decreased_in_magnitude_relative_to"
    "PATO:has_cross_section"
    "PATO:has_relative_magnitude"
    "PATO:increased_in_magnitude_relative_to"
    "PATO:reciprocal_of"
    "PATO:towards"
    "PDQ:associated_disease"
    "PDQ:component_of"
    "PDQ:has_component"
    "PR:has_gene_template"
    "PR:lacks_part"
    "PR:non-covalently_bound_to"
    "PathWhiz:has_bound"
    "PathWhiz:has_compound"
    "PathWhiz:has_element_collection"
    "PathWhiz:has_element_in_bound"
    "PathWhiz:has_enzyme"
    "PathWhiz:has_left_element"
    "PathWhiz:has_location"
    "PathWhiz:has_nucleic_acid"
    "PathWhiz:has_protein"
    "PathWhiz:has_protein_in_complex"
    "PathWhiz:has_reaction"
    "PathWhiz:has_right_element"
    "PathWhiz:in_species"
    "REPODB:clinically_tested_approved_unknown_phase"
    "REPODB:clinically_tested_suspended_phase_0"
    "REPODB:clinically_tested_suspended_phase_1"
    "REPODB:clinically_tested_suspended_phase_1_or_phase_2"
    "REPODB:clinically_tested_suspended_phase_2"
    "REPODB:clinically_tested_suspended_phase_2_or_phase_3"
    "REPODB:clinically_tested_suspended_phase_3"
    "REPODB:clinically_tested_terminated_phase_0"
    "REPODB:clinically_tested_terminated_phase_1"
    "REPODB:clinically_tested_terminated_phase_1_or_phase_2"
    "REPODB:clinically_tested_terminated_phase_2"
    "REPODB:clinically_tested_terminated_phase_2_or_phase_3"
    "REPODB:clinically_tested_terminated_phase_3"
    "REPODB:clinically_tested_withdrawn_phase_0"
    "REPODB:clinically_tested_withdrawn_phase_1"
    "REPODB:clinically_tested_withdrawn_phase_1_or_phase_2"
    "REPODB:clinically_tested_withdrawn_phase_2"
    "REPODB:clinically_tested_withdrawn_phase_2_or_phase_3"
    "REPODB:clinically_tested_withdrawn_phase_3"
    "RO:0000052"
    "RO:0000053"
    "RO:0000056"
    "RO:0000057"
    "RO:0000086"
    "RO:0000087"
    "RO:0001000"
    "RO:0001015"
    "RO:0001019"
    "RO:0001022"
    "RO:0001025"
    "RO:0002001"
    "RO:0002002"
    "RO:0002005"
    "RO:0002007"
    "RO:0002008"
    "RO:0002082"
    "RO:0002083"
    "RO:0002087"
    "RO:0002090"
    "RO:0002092"
    "RO:0002093"
    "RO:0002100"
    "RO:0002102"
    "RO:0002103"
    "RO:0002104"
    "RO:0002120"
    "RO:0002130"
    "RO:0002131"
    "RO:0002134"
    "RO:0002150"
    "RO:0002159"
    "RO:0002160"
    "RO:0002162"
    "RO:0002170"
    "RO:0002176"
    "RO:0002177"
    "RO:0002178"
    "RO:0002179"
    "RO:0002180"
    "RO:0002200"
    "RO:0002202"
    "RO:0002203"
    "RO:0002211"
    "RO:0002212"
    "RO:0002213"
    "RO:0002215"
    "RO:0002216"
    "RO:0002219"
    "RO:0002220"
    "RO:0002221"
    "RO:0002223"
    "RO:0002224"
    "RO:0002225"
    "RO:0002226"
    "RO:0002229"
    "RO:0002230"
    "RO:0002231"
    "RO:0002232"
    "RO:0002233"
    "RO:0002234"
    "RO:0002254"
    "RO:0002256"
    "RO:0002285"
    "RO:0002292"
    "RO:0002295"
    "RO:0002296"
    "RO:0002297"
    "RO:0002298"
    "RO:0002299"
    "RO:0002303"
    "RO:0002309"
    "RO:0002313"
    "RO:0002314"
    "RO:0002315"
    "RO:0002322"
    "RO:0002328"
    "RO:0002331"
    "RO:0002332"
    "RO:0002334"
    "RO:0002338"
    "RO:0002339"
    "RO:0002340"
    "RO:0002341"
    "RO:0002342"
    "RO:0002343"
    "RO:0002344"
    "RO:0002345"
    "RO:0002348"
    "RO:0002349"
    "RO:0002350"
    "RO:0002351"
    "RO:0002352"
    "RO:0002353"
    "RO:0002354"
    "RO:0002355"
    "RO:0002356"
    "RO:0002371"
    "RO:0002372"
    "RO:0002373"
    "RO:0002374"
    "RO:0002376"
    "RO:0002380"
    "RO:0002385"
    "RO:0002387"
    "RO:0002411"
    "RO:0002412"
    "RO:0002433"
    "RO:0002451"
    "RO:0002473"
    "RO:0002488"
    "RO:0002489"
    "RO:0002491"
    "RO:0002492"
    "RO:0002494"
    "RO:0002495"
    "RO:0002496"
    "RO:0002497"
    "RO:0002500"
    "RO:0002505"
    "RO:0002507"
    "RO:0002509"
    "RO:0002524"
    "RO:0002551"
    "RO:0002565"
    "RO:0002568"
    "RO:0002571"
    "RO:0002572"
    "RO:0002573"
    "RO:0002576"
    "RO:0002578"
    "RO:0002588"
    "RO:0002590"
    "RO:0002591"
    "RO:0002592"
    "RO:0002608"
    "RO:0002629"
    "RO:0002630"
    "RO:0003000"
    "RO:0003001"
    "RO:0003304"
    "RO:0004001"
    "RO:0004007"
    "RO:0004008"
    "RO:0004009"
    "RO:0004019"
    "RO:0004020"
    "RO:0004021"
    "RO:0004022"
    "RO:0004024"
    "RO:0004025"
    "RO:0004026"
    "RO:0004027"
    "RO:0004028"
    "RO:0004029"
    "RO:0004030"
    "RO:0009001"
    "RO:0009004"
    "RO:0009501"
    "RO:0012003"
    "RO:derives_from"
    "RO:has_participant"
    "RO:participates_in"
    "RTXKG1:affects"
    "RTXKG1:associated_with_disease"
    "RTXKG1:capable_of"
    "RTXKG1:capable_of_part_of"
    "RTXKG1:contains_process"
    "RTXKG1:contraindicated_for"
    "RTXKG1:disease_caused_by_disruption_of"
    "RTXKG1:disease_causes_disruption_of"
    "RTXKG1:disease_has_basis_in_dysfunction_of"
    "RTXKG1:disease_has_location"
    "RTXKG1:expressed_in"
    "RTXKG1:gene_associated_with_condition"
    "RTXKG1:gene_mutations_contribute_to"
    "RTXKG1:has_phenotype"
    "RTXKG1:has_plasma_membrane_part"
    "RTXKG1:indicated_for"
    "RTXKG1:involved_in"
    "RTXKG1:lacks_part"
    "RTXKG1:participates_in"
    "RTXKG1:physically_interacts_with"
    "RTXKG1:realized_in_response_to"
    "RTXKG1:realized_in_response_to_stimulus"
    "RTXKG1:regulates_activity_of"
    "RTXKG1:regulates_expression_of"
    "RTXKG1:site_of"
    "RTXKG1:subclass_of"
    "RTXKG1:targets"
    "RXNORM:consists_of"
    "RXNORM:constitutes"
    "RXNORM:contained_in"
    "RXNORM:contains"
    "RXNORM:has_dose_form"
    "RXNORM:has_doseformgroup"
    "RXNORM:has_form"
    "RXNORM:has_ingredient"
    "RXNORM:has_part"
    "RXNORM:has_quantified_form"
    "RXNORM:has_tradename"
    "RXNORM:ingredient_of"
    "RXNORM:ingredients_of"
    "RXNORM:inverse_isa"
    "RXNORM:isa"
    "RXNORM:part_of"
    "RXNORM:precise_ingredient_of"
    "RXNORM:reformulated_to"
    "SEMMEDDB:administered_to"
    "SEMMEDDB:affects"
    "SEMMEDDB:associated_with"
    "SEMMEDDB:augments"
    "SEMMEDDB:causes"
    "SEMMEDDB:coexists_with"
    "SEMMEDDB:compared_with"
    "SEMMEDDB:complicates"
    "SEMMEDDB:converts_to"
    "SEMMEDDB:diagnoses"
    "SEMMEDDB:disrupts"
    "SEMMEDDB:inhibits"
    "SEMMEDDB:interacts_with"
    "SEMMEDDB:isa"
    "SEMMEDDB:location_of"
    "SEMMEDDB:manifestation_of"
    "SEMMEDDB:measures"
    "SEMMEDDB:occurs_in"
    "SEMMEDDB:part_of"
    "SEMMEDDB:precedes"
    "SEMMEDDB:predisposes"
    "SEMMEDDB:prevents"
    "SEMMEDDB:process_of"
    "SEMMEDDB:produces"
    "SEMMEDDB:same_as"
    "SEMMEDDB:stimulates"
    "SEMMEDDB:treats"
    "SEMMEDDB:uses"
    "SNOMED:active_ingredient_of"
    "SNOMED:associated_with"
    "SNOMED:basis_of_strength_substance_of"
    "SNOMED:causative_agent_of"
    "SNOMED:cause_of"
    "SNOMED:characterized_by"
    "SNOMED:component_of"
    "SNOMED:direct_substance_of"
    "SNOMED:during"
    "SNOMED:entire_anatomy_structure_of"
    "SNOMED:focus_of"
    "SNOMED:has_access"
    "SNOMED:has_associated_finding"
    "SNOMED:has_associated_morphology"
    "SNOMED:has_associated_procedure"
    "SNOMED:has_basic_dose_form"
    "SNOMED:has_clinical_course"
    "SNOMED:has_component"
    "SNOMED:has_concentration_strength_denominator_unit"
    "SNOMED:has_concentration_strength_numerator_unit"
    "SNOMED:has_concentration_strength_numerator_value"
    "SNOMED:has_count_of_base_of_active_ingredient"
    "SNOMED:has_definitional_manifestation"
    "SNOMED:has_dependent"
    "SNOMED:has_direct_device"
    "SNOMED:has_direct_morphology"
    "SNOMED:has_direct_procedure_site"
    "SNOMED:has_direct_site"
    "SNOMED:has_disposition"
    "SNOMED:has_dose_form"
    "SNOMED:has_dose_form_administration_method"
    "SNOMED:has_dose_form_intended_site"
    "SNOMED:has_dose_form_release_characteristic"
    "SNOMED:has_dose_form_transformation"
    "SNOMED:has_finding_context"
    "SNOMED:has_finding_informer"
    "SNOMED:has_finding_method"
    "SNOMED:has_finding_site"
    "SNOMED:has_indirect_device"
    "SNOMED:has_indirect_morphology"
    "SNOMED:has_indirect_procedure_site"
    "SNOMED:has_inherent_attribute"
    "SNOMED:has_inherent_location"
    "SNOMED:has_intent"
    "SNOMED:has_interpretation"
    "SNOMED:has_laterality"
    "SNOMED:has_measurement_method"
    "SNOMED:has_method"
    "SNOMED:has_modification"
    "SNOMED:has_occurrence"
    "SNOMED:has_pathological_process"
    "SNOMED:has_precondition"
    "SNOMED:has_presentation_strength_denominator_unit"
    "SNOMED:has_presentation_strength_denominator_value"
    "SNOMED:has_presentation_strength_numerator_unit"
    "SNOMED:has_presentation_strength_numerator_value"
    "SNOMED:has_priority"
    "SNOMED:has_procedure_context"
    "SNOMED:has_procedure_device"
    "SNOMED:has_procedure_morphology"
    "SNOMED:has_procedure_site"
    "SNOMED:has_process_duration"
    "SNOMED:has_process_output"
    "SNOMED:has_property"
    "SNOMED:has_realization"
    "SNOMED:has_recipient_category"
    "SNOMED:has_revision_status"
    "SNOMED:has_route_of_administration"
    "SNOMED:has_scale_type"
    "SNOMED:has_severity"
    "SNOMED:has_specimen"
    "SNOMED:has_specimen_source_identity"
    "SNOMED:has_specimen_source_morphology"
    "SNOMED:has_specimen_source_topography"
    "SNOMED:has_specimen_substance"
    "SNOMED:has_state_of_matter"
    "SNOMED:has_subject_relationship_context"
    "SNOMED:has_surgical_approach"
    "SNOMED:has_technique"
    "SNOMED:has_temporal_context"
    "SNOMED:has_time_aspect"
    "SNOMED:has_unit_of_presentation"
    "SNOMED:has_units"
    "SNOMED:inherent_location_of"
    "SNOMED:inheres_in"
    "SNOMED:is_interpreted_by"
    "SNOMED:occurs_before"
    "SNOMED:occurs_in"
    "SNOMED:part_anatomy_structure_of"
    "SNOMED:plays_role"
    "SNOMED:precise_active_ingredient_of"
    "SNOMED:relative_to_part_of"
    "SNOMED:specimen_procedure_of"
    "SNOMED:temporally_related_to"
    "SNOMED:uses_access_device"
    "SNOMED:uses_device"
    "SNOMED:uses_energy"
    "SNOMED:uses_substance"
    "SO:adjacent_to"
    "SO:contains"
    "SO:derives_from"
    "SO:guided_by"
    "SO:has_origin"
    "SO:has_part"
    "SO:has_quality"
    "SO:member_of"
    "SO:non_functional_homolog_of"
    "SO:overlaps"
    "SO:transcribed_from"
    "SO:transcribed_to"
    "UBERON:anastomoses_with"
    "UBERON:anteriorly_connected_to"
    "UBERON:channel_for"
    "UBERON:channels_from"
    "UBERON:channels_into"
    "UBERON:conduit_for"
    "UBERON:distally_connected_to"
    "UBERON:extends_fibers_into"
    "UBERON:filtered_through"
    "UBERON:in_central_side_of"
    "UBERON:in_innermost_side_of"
    "UBERON:in_outermost_side_of"
    "UBERON:indirectly_supplies"
    "UBERON:posteriorly_connected_to"
    "UBERON:protects"
    "UBERON:proximally_connected_to"
    "UBERON:sexually_homologous_to"
    "UBERON:site_of"
    "UBERON:subdivision_of"
    "UBERON:synapsed_by"
    "UMLS:RB"
    "UMLS:RO"
    "UMLS:RQ"
    "UMLS:SY"
    "UMLS:class_code_classified_by"
    "UMLS:component_of"
    "UMLS:exhibited_by"
    "UMLS:has_component"
    "UMLS:has_context_binding"
    "UMLS:has_form"
    "UMLS:has_mapping_qualifier"
    "UMLS:has_owning_affiliate"
    "UMLS:has_physiologic_effect"
    "UMLS:has_structural_class"
    "UMLS:has_supported_concept_property"
    "UMLS:has_supported_concept_relationship"
    "UMLS:larger_than"
    "UMLS:mapped_to"
    "UMLS:may_be_qualified_by"
    "UMLS:measures"
    "UMLS:owning_section_of"
    "UMLS:owning_subsection_of"
    "UMLS:related_to"
    "VANDF:has_ingredient"
    "VANDF:ingredient_of"
    "VANDF:inverse_isa"
    "VANDF:isa"
    "biolink:has_gene_product"
    "biolink:in_taxon"
    "biolink:part_of"
    "biolink:physically_interacts_with"
    "biolink:related_to"
    "biolink:same_as"
    "biolink:subclass_of"
    "biolink:treats"
    "oboFormat:xref"
    "owl:sameAs"
    "rdfs:subClassOf"
    "rdfs:subPropertyOf"))

(test 'single-concept-properties
  (run* (k v)
    (cprop "UMLS:C0000137" k v))
  '(("category" "biolink:ChemicalSubstance")
    ("category_label" "chemical_substance")
    ("deprecated" "False")
    ("description"
     "The most abundant form of RNA. Together with proteins, it forms the ribosomes, playing a structural role and also a role in ribosomal binding of mRNA and tRNAs. Individual chains are conventionally designated by their sedimentation coefficients. In eukaryotes, four large chains exist, synthesized in the nucleolus and constituting about 50% of the ribosome. (Dorland, 28th ed)")
    ("id" "UMLS:C0000137")
    ("iri" "https://identifiers.org/umls:C0000137")
    ("name" "RNA, Ribosomal")
    ("provided_by" "identifiers_org_registry:umls")
    ("update_date" "2015")))

(test 'single-subject-edges
  (run 5 (id s o) (== s "UMLS:C0005767") (edge id s o))
  '((17304173 "UMLS:C0005767" "NCBIGene:102723407")
    (15656350 "UMLS:C0005767" "NCBIGene:10312")
    (19424627 "UMLS:C0005767" "NCBIGene:11168")
    (26022613 "UMLS:C0005767" "NCBIGene:1154")
    (29233569 "UMLS:C0005767" "NCBIGene:115727")))

(test 'single-object-edges
  (run 5 (id s o) (== o "UMLS:C0005767") (edge id s o))
  '((75693 "FMA:9670" "UMLS:C0005767")
    (48607873 "HMDB:HMDB0000001" "UMLS:C0005767")
    (48607894 "HMDB:HMDB0000002" "UMLS:C0005767")
    (48607928 "HMDB:HMDB0000005" "UMLS:C0005767")
    (48607976 "HMDB:HMDB0000008" "UMLS:C0005767")))

(test 'single-edge
  (run* (id s o)
    (== s "UMLS:C0005767")
    (== o "NCBIGene:3688")
    (edge id s o))
  '((25672055 "UMLS:C0005767" "NCBIGene:3688")))

(test 'single-edge-properties
  (run* (id k v)
    (edge id "UMLS:C0005767" "NCBIGene:3688")
    (eprop id k v))
  '((25672055 "negated" "False")
    (25672055 "object" "NCBIGene:3688")
    (25672055 "predicate" "biolink:produces")
    (25672055 "predicate_label" "produces")
    (25672055 "provided_by" "\"(\\\"\\\"SEMMEDDB:\\\"\\\")\"")
    (25672055 "publications" "\"(\\\"\\\"PMID:21182179\\\"\\\")\"")
    (25672055
     "publications_info"
     "{'PMID:21182179': {'publication date': '2011 Jan', 'sentence': 'In particular, although CD34(Pos)CD45(Dim)CD38(Pos) HSCs from both hEPCB and hTCB expressed relatively higher amounts of CD29, CD71, and CD135 compared with CD34(Pos)CD45(Dim)CD38(Neg) cells, a higher expression of CD31 was restricted to CD34(Pos)CD45(Dim)CD38(Pos) cells from hEPCB samples, and a higher expression of CD117 was demonstrated in CD34(Pos)CD45(Dim)CD38(Pos) cells from hTCB samples.', 'subject score': 762, 'object score': 1000}}")
    (25672055 "relation" "SEMMEDDB:produces")
    (25672055 "relation_label" "produces")
    (25672055 "subject" "UMLS:C0005767")
    (25672055 "update_date" "2019-05-01 15:34:14")))

(test 'concept-descriptions
  (run 3 (c cat n desc)
    (cprop c "category" cat)
    (cprop c "name" n)
    (cprop c "description" desc))
  '(("VT:0002292"
     "biolink:InformationContentEntity"
     "gestation period duration"
     "         The length of time between when a fertilized egg implants in the wall of the uterus and the birth of offspring.         ")
    ("RO:0001902"
     "biolink:RelationshipType"
     "relation has no temporal argument"
     "  ## Elucidation  This is used when the first-order logic form of the relation is binary, and takes no temporal argument.  ## Example:      Class: limb     SubClassOf: develops_from some lateral-plate-mesoderm       forall t, t2:       forall x :         instance_of(x,Limb,t)          implies         exists y :           instance_of(y,LPM,t2)           develops_from(x,y)   ")
    ("PathWhiz:PW000166"
     "biolink:Pathway"
     "Threonine and 2-Oxobutanoate Degradation"
     "  2-oxobutanoate, also known as 2-Ketobutyric acid, is a 2-keto acid that is commonly produced in the metabolism of amino acids such as methionine and threonine.  Like other 2-keto acids, degradation of 2-oxobutanoate occurs in the mitochondrial matrix and begins with oxidative decarboxylation to its acyl coenzyme A derivative, propionyl-CoA.  This reaction is mediated by a class of large, multienzyme complexes called 2-oxo acid dehydrogenase complexes.  While no 2-oxo acid dehydrogenase complex is specific to 2-oxobutanoate, numerous complexes can catalyze its reaction.  In this pathway the branched-chain alpha-keto acid dehydrogenase complex is depicted.  All 2-oxo acid dehydrogenase complexes consist of three main components: a 2-oxo acid dehydrogenase (E1) with a thiamine pyrophosphate cofactor, a dihydrolipoamide acyltransferase (E2) with a lipoate cofactor, and a dihydrolipoamide dehydrogenase (E3) with a flavin cofactor.  E1 binds the 2-oxobutanoate to the lipoate on E2, which then transfers the propionyl group to coenzyme A, producing propionyl-CoA and reducing the lipoate.  E3 then transfers protons to NAD in order to restore the lipoate.  Propionyl-CoA carboxylase transforms the propionyl-CoA to S-methylmalonyl-CoA, which is then converted to R-methylmalonyl-CoA via methylmalonyl-CoA epimerase.  In the final step, methylmalonyl-CoA mutase acts on the R-methylmalonyl-CoA to produce succinyl-CoA. ")))

(test 'chemical-substance-names
  (run 10 (curie name)
    (cprop curie "category" "biolink:ChemicalSubstance")
    (cprop curie "name" name))
  '(("ATC:A01AA02" "sodium monofluorophosphate")
    ("ATC:A01AA03" "olaflur")
    ("ATC:A01AB02" "hydrogen peroxide")
    ("ATC:A01AB03" "chlorhexidine")
    ("ATC:A01AB04" "amphotericin B")
    ("ATC:A01AB05" "polynoxylin")
    ("ATC:A01AB06" "domiphen")
    ("ATC:A01AB07" "oxyquinoline")
    ("ATC:A01AB08" "neomycin")
    ("ATC:A01AB09" "miconazole")))

(test 'gene-names
  (run 10 (c n)
    (cprop c "name"     n)
    (cprop c "category" "biolink:Gene"))
  '(("AraPort:AT1G01010" "NAC001 (Arabidopsis thaliana)")
    ("AraPort:AT1G01020" "ARV1 (Arabidopsis thaliana)")
    ("AraPort:AT1G01030" "NGA3 (Arabidopsis thaliana)")
    ("AraPort:AT1G01040" "DCL1 (Arabidopsis thaliana)")
    ("AraPort:AT1G01050" "PPA1 (Arabidopsis thaliana)")
    ("AraPort:AT1G01060" "LHY (Arabidopsis thaliana)")
    ("AraPort:AT1G01070" "At1g01070 (Arabidopsis thaliana)")
    ("AraPort:AT1G01090" "PDH-E1 ALPHA (Arabidopsis thaliana)")
    ("AraPort:AT1G01100" "RPP1A (Arabidopsis thaliana)")
    ("AraPort:AT1G01120" "KCS1 (Arabidopsis thaliana)")))

(test 'concept-relationships
  (run 5 (curie1 predicate curie2)
    (fresh (eid)
      (eprop eid "predicate" predicate)
      (edge eid curie1 curie2)))
  '(("NCIT:C18585" "biolink:actively_involved_in" "NCIT:C45399")
    ("NCIT:C18363" "biolink:actively_involved_in" "NCIT:C158854")
    ("NCIT:C24399" "biolink:actively_involved_in" "NCIT:C158823")
    ("NCIT:C24399" "biolink:actively_involved_in" "NCIT:C158822")
    ("NCIT:C20719" "biolink:actively_involved_in" "NCIT:C158822")))

(test 'concept-regulators
  (run 5 (curie1 predicate curie2)
    (fresh (eid)
      (membero predicate '("biolink:negatively_regulates"
                           "biolink:negatively_regulates,_entity_to_entity"
                           "biolink:negatively_regulates,_process_to_process"
                           "biolink:positively_regulates"
                           "biolink:positively_regulates,_entity_to_entity"
                           "biolink:positively_regulates,_process_to_process"
                           "biolink:regulates"
                           "biolink:regulates,_process_to_process"))
      (eprop eid "predicate" predicate)
      (edge eid curie1 curie2)))
  '(("GO:0002862" "biolink:negatively_regulates" "GO:0002437")
    ("GO:0002875" "biolink:negatively_regulates" "GO:0002439")
    ("GO:0002865" "biolink:negatively_regulates" "GO:0002438")
    ("GO:0002632" "biolink:negatively_regulates" "GO:0002432")
    ("GO:0002707" "biolink:negatively_regulates" "GO:0002449")))

(test 'specific-concept-regulators
  (run 20 (curie1 predicate curie2)
    (fresh (eid)
      (membero predicate '("biolink:negatively_regulates"
                           "biolink:negatively_regulates,_entity_to_entity"
                           "biolink:negatively_regulates,_process_to_process"
                           "biolink:positively_regulates"
                           "biolink:positively_regulates,_entity_to_entity"
                           "biolink:positively_regulates,_process_to_process"
                           "biolink:regulates"
                           "biolink:regulates,_process_to_process"))
      (eprop eid "predicate" predicate)
      (== curie2 "GO:0002437")
      (edge eid curie1 curie2)))
  '(("GO:0002862" "biolink:negatively_regulates" "GO:0002437")
    ("GO:0002862" "biolink:negatively_regulates,_process_to_process" "GO:0002437")
    ("GO:0002863" "biolink:positively_regulates" "GO:0002437")
    ("GO:0002863" "biolink:positively_regulates,_process_to_process" "GO:0002437")
    ("GO:0002861" "biolink:regulates" "GO:0002437")
    ("GO:0002861" "biolink:regulates,_process_to_process" "GO:0002437")))

(test 'named-negative-regulators
  (run 5 (curie1 name1 curie2 name2)
    (fresh (eid)
      (eprop eid "predicate" "biolink:negatively_regulates")
      (edge eid curie1 curie2)
      (cprop curie1 "name" name1)
      (cprop curie2 "name" name2)))
  '(("GO:0002862"
     "negative regulation of inflammatory response to antigenic stimulus"
     "GO:0002437"
     "inflammatory response to antigenic stimulus")
    ("GO:0002875"
     "negative regulation of chronic inflammatory response to antigenic stimulus"
     "GO:0002439"
     "chronic inflammatory response to antigenic stimulus")
    ("GO:0002865"
     "negative regulation of acute inflammatory response to antigenic stimulus"
     "GO:0002438"
     "acute inflammatory response to antigenic stimulus")
    ("GO:0002632"
     "negative regulation of granuloma formation"
     "GO:0002432"
     "granuloma formation")
    ("GO:0002707"
     "negative regulation of lymphocyte mediated immunity"
     "GO:0002449"
     "lymphocyte mediated immunity")))

(test 'nausea-properties
  (run* (k v)
    (cprop "UMLS:C0520909" k v))
  '(("category" "biolink:Disease")
    ("category_label" "disease")
    ("deprecated" "False")
    ("description" "Emesis and queasiness occurring after anesthesia.")
    ("id" "UMLS:C0520909")
    ("iri" "https://identifiers.org/umls:C0520909")
    ("name" "Postoperative nausea and vomiting")
    ("provided_by" "identifiers_org_registry:umls")
    ("update_date" "2017")))

(test 'nausea-object-edges
  (run 5 (s cat name p)
    (fresh (eid)
      (edge eid s "UMLS:C0520909")
      (cprop s "category" cat)
      (cprop s "name" name)
      (eprop eid "predicate" p)))
  '(("DRUGBANK:DB00184" "biolink:Drug" "Nicotine" "biolink:treats")
    ("MESH:D020250"
     "biolink:NamedThing"
     "Postoperative Nausea and Vomiting"
     "biolink:close_match")
    ("NCBIGene:166"
     "biolink:Gene"
     "TLE family member 5, transcriptional modulator"
     "biolink:related_to")
    ("NCBIGene:1814" "biolink:Gene" "dopamine receptor D3" "biolink:related_to")
    ("NCBIGene:2770"
     "biolink:Gene"
     "G protein subunit alpha i1"
     "biolink:related_to")))

(test 'nausea-treatments
  (run* (s cat name)
    (fresh (eid)
      (edge eid s "UMLS:C0520909")
      (cprop s "category" cat)
      (cprop s "name" name)
      (eprop eid "predicate" "biolink:treats")))
  '(("DRUGBANK:DB00184" "biolink:Drug" "Nicotine")
    ("UMLS:C0001425" "biolink:Procedure" "Adenoid excision")
    ("UMLS:C0001563" "biolink:Procedure" "Medication administration: oral")
    ("UMLS:C0001617" "biolink:ChemicalSubstance" "Adrenal corticosteriods")
    ("UMLS:C0002428" "biolink:Procedure" "Ambulatory surgery")
    ("UMLS:C0002766" "biolink:Procedure" "Pain management")
    ("UMLS:C0002769" "biolink:Procedure" "epidural analgesia")
    ("UMLS:C0002903" "biolink:Procedure" "Administration of anesthesia")
    ("UMLS:C0002912" "biolink:Procedure" "Anesthesia, Dental")
    ("UMLS:C0002915" "biolink:Procedure" "General anesthesia")
    ("UMLS:C0002917" "biolink:Procedure" "Anesthesia, Inhalation")
    ("UMLS:C0002928" "biolink:Procedure" "Spinal anesthesia")
    ("UMLS:C0002932" "biolink:ChemicalSubstance" "Anesthetics")
    ("UMLS:C0003297" "biolink:ChemicalSubstance" "Antiemetics")
    ("UMLS:C0003893" "biolink:Procedure" "Arthroplasty")
    ("UMLS:C0004933" "biolink:Procedure" "Behavioral therapy")
    ("UMLS:C0005064" "biolink:NamedThing" "Benzodiazepine")
    ("UMLS:C0008320" "biolink:Procedure" "Cholecystectomy")
    ("UMLS:C0009014" "biolink:ChemicalSubstance" "Clonidine")
    ("UMLS:C0009429" "biolink:Procedure" "Combination therapy")
    ("UMLS:C0009653" "biolink:Device" "Condom")
    ("UMLS:C0010280" "biolink:Procedure" "Craniotomy")
    ("UMLS:C0011331" "biolink:Procedure" "Dental care")
    ("UMLS:C0011777" "biolink:ChemicalSubstance" "Dexamethasone")
    ("UMLS:C0012358" "biolink:Procedure" "Dilation and curettage of uterus")
    ("UMLS:C0012381" "biolink:ChemicalSubstance" "Dimenhydrinate")
    ("UMLS:C0013136" "biolink:ChemicalSubstance" "Droperidol")
    ("UMLS:C0013216" "biolink:Procedure" "Drug therapy")
    ("UMLS:C0013227"
     "biolink:ChemicalSubstance"
     "Pharmaceutical / biologic product")
    ("UMLS:C0013973" "biolink:ChemicalSubstance" "Emetics")
    ("UMLS:C0017118" "biolink:Procedure" "Gastrectomy")
    ("UMLS:C0018546" "biolink:ChemicalSubstance" "Haloperidol")
    ("UMLS:C0020591" "biolink:ChemicalSubstance" "Hypnotic agent")
    ("UMLS:C0020699" "biolink:Procedure" "Hysterectomy")
    ("UMLS:C0020700" "biolink:Procedure" "Vaginal hysterectomy")
    ("UMLS:C0021778"
     "biolink:Procedure"
     "Intermittent positive pressure ventilation")
    ("UMLS:C0021925" "biolink:Procedure" "Intubation - action")
    ("UMLS:C0022237" "biolink:ChemicalSubstance" "Alcohol,isopropyl")
    ("UMLS:C0024881" "biolink:Procedure" "Mastectomies")
    ("UMLS:C0024883" "biolink:Procedure" "Modified radical mastectomy")
    ("UMLS:C0025619" "biolink:ChemicalSubstance" "Mesylates")
    ("UMLS:C0025853" "biolink:ChemicalSubstance" "Metoclopramide")
    ("UMLS:C0026056" "biolink:ChemicalSubstance" "Midazolam")
    ("UMLS:C0026549" "biolink:ChemicalSubstance" "Morphine")
    ("UMLS:C0026868" "biolink:Procedure" "Music therapy")
    ("UMLS:C0027136" "biolink:Procedure" "Myringoplasty")
    ("UMLS:C0030054" "biolink:ChemicalSubstance" "Oxygen")
    ("UMLS:C0032042" "biolink:Procedure" "Placebos")
    ("UMLS:C0033229" "biolink:ChemicalSubstance" "Prochlorperazine")
    ("UMLS:C0033231" "biolink:ChemicalSubstance" "Prochlorperazine maleate")
    ("UMLS:C0033405" "biolink:ChemicalSubstance" "Promethazine")
    ("UMLS:C0033487" "biolink:ChemicalSubstance" "Propofol")
    ("UMLS:C0036442" "biolink:ChemicalSubstance" "Scopolamine")
    ("UMLS:C0036751" "biolink:NamedThing" "Serotonin")
    ("UMLS:C0038901" "biolink:Procedure" "Surgical procedure on eye proper")
    ("UMLS:C0038902" "biolink:Procedure" "Operation on female genital organs")
    ("UMLS:C0038910"
     "biolink:Procedure"
     "Otorhinolaryngologic Surgical Procedures")
    ("UMLS:C0040145" "biolink:Procedure" "Thyroidectomy")
    ("UMLS:C0040423" "biolink:Procedure" "Tonsillectomy")
    ("UMLS:C0040508" "biolink:Procedure" "Total replacement of hip")
    ("UMLS:C0040610" "biolink:ChemicalSubstance" "Tramadol")
    ("UMLS:C0040654"
     "biolink:Procedure"
     "Percutaneous electrical nerve stimulation")
    ("UMLS:C0040808" "biolink:NamedThing" "Abdominal Wall")
    ("UMLS:C0041447" "biolink:Procedure" "Repair of middle ear")
    ("UMLS:C0051162" "biolink:NamedThing" "Alizapride")
    ("UMLS:C0053139" "biolink:ChemicalSubstance" "Benzamide")
    ("UMLS:C0061851" "biolink:ChemicalSubstance" "Ondansetron")
    ("UMLS:C0061863" "biolink:ChemicalSubstance" "Granisetron")
    ("UMLS:C0063322" "biolink:NamedThing" "Tropisetron")
    ("UMLS:C0065818" "biolink:ChemicalSubstance" "Dolasetron mesylate")
    ("UMLS:C0078944" "biolink:Procedure" "Patient controlled analgesia")
    ("UMLS:C0086511" "biolink:Procedure" "Arthroplasty of knee")
    ("UMLS:C0086930" "biolink:Procedure" "Risk assessment")
    ("UMLS:C0087111" "biolink:Procedure" "Therapy")
    ("UMLS:C0103045" "biolink:NamedThing" "Amisulpride-containing product")
    ("UMLS:C0150521" "biolink:Procedure" "Comfort measures")
    ("UMLS:C0162522" "biolink:Procedure" "Laparoscopic cholecystectomy")
    ("UMLS:C0162561" "biolink:Procedure" "Catheter Ablation")
    ("UMLS:C0163712" "biolink:ChemicalSubstance" "Relate - vinyl resin")
    ("UMLS:C0180288" "biolink:Publication" "Anesthesia information system")
    ("UMLS:C0184625" "biolink:Activity" "Normal diet")
    ("UMLS:C0184661" "biolink:Procedure" "Procedure")
    ("UMLS:C0185624" "biolink:IndividualOrganism" "Orthognathic Surgery")
    ("UMLS:C0188970" "biolink:Procedure" "Operation on nose")
    ("UMLS:C0191922" "biolink:Procedure" "Reduction mammoplasty")
    ("UMLS:C0192817" "biolink:Procedure" "Operation on colon")
    ("UMLS:C0192866" "biolink:Procedure" "Sigmoid colectomy")
    ("UMLS:C0193769" "biolink:Procedure" "Operation on thyroid gland")
    ("UMLS:C0193788" "biolink:Procedure" "Total thyroidectomy")
    ("UMLS:C0197981" "biolink:Procedure" "Strabismus surgery")
    ("UMLS:C0198154" "biolink:Procedure" "Mastoidectomy")
    ("UMLS:C0198482" "biolink:Procedure" "Operation on abdominal region")
    ("UMLS:C0199176" "biolink:Procedure" "Prophylaxis - intent")
    ("UMLS:C0206046" "biolink:NamedThing" "Zofran")
    ("UMLS:C0206058" "biolink:Procedure" "Optional surgery")
    ("UMLS:C0209210" "biolink:ChemicalSubstance" "Dolasetron")
    ("UMLS:C0209337" "biolink:NamedThing" "Rocuronium")
    ("UMLS:C0220578" "biolink:ChemicalSubstance" "Palonosetron")
    ("UMLS:C0242402" "biolink:NamedThing" "Opioid receptor agonist")
    ("UMLS:C0242702"
     "biolink:ChemicalSubstance"
     "Dopamine receptor antagonist-containing product")
    ("UMLS:C0242897" "biolink:ChemicalSubstance" "Anticholinergic Agent")
    ("UMLS:C0243076" "biolink:ChemicalSubstance" "antagonists & inhibitors")
    ("UMLS:C0278259" "biolink:Procedure" "Local excision")
    ("UMLS:C0282046" "biolink:Procedure" "Ambulatory Surgical Procedures")
    ("UMLS:C0282493" "biolink:Procedure" "Endoscopy with surgical procedure")
    ("UMLS:C0282614" "biolink:Procedure" "Acupressure")
    ("UMLS:C0360055"
     "biolink:NamedThing"
     "5-HT3 receptor antagonist-containing product")
    ("UMLS:C0376547" "biolink:Procedure" "Aromatherapy")
    ("UMLS:C0394663" "biolink:Procedure" "Cupping")
    ("UMLS:C0394664" "biolink:Procedure" "Acupuncture")
    ("UMLS:C0404077" "biolink:Procedure" "Abdominal hysterectomy")
    ("UMLS:C0404079" "biolink:Procedure" "Total abdominal hysterectomy")
    ("UMLS:C0404088"
     "biolink:Procedure"
     "Laparoscopic-assisted vaginal hysterectomy")
    ("UMLS:C0404089" "biolink:Procedure" "Laparoscopic hysterectomy")
    ("UMLS:C0404090" "biolink:Procedure" "Laparoscopic total hysterectomy")
    ("UMLS:C0408578" "biolink:Procedure" "Operation on lumbar spine")
    ("UMLS:C0420172" "biolink:Procedure" "Drug prophylaxis")
    ("UMLS:C0442968" "biolink:Procedure" "Functional endoscopic sinus surgery")
    ("UMLS:C0450442" "biolink:ChemicalSubstance" "Agent")
    ("UMLS:C0473965" "biolink:Procedure" "Total intravenous anesthesia")
    ("UMLS:C0520483" "biolink:Procedure" "Ligation of fallopian tube")
    ("UMLS:C0524850" "biolink:Procedure" "Operation on nervous system")
    ("UMLS:C0526950" "biolink:ChemicalSubstance" "ramosetron")
    ("UMLS:C0543467" "biolink:Procedure" "Surgery")
    ("UMLS:C0543476" "biolink:ChemicalSubstance" "Granisetron hydrochloride")
    ("UMLS:C0677616" "biolink:Procedure" "Plastic operation")
    ("UMLS:C0700478" "biolink:ChemicalSubstance" "Ondansetron hydrochloride")
    ("UMLS:C0728940" "biolink:Procedure" "Surgical removal")
    ("UMLS:C0750934" "biolink:Procedure" "Arthroscopy with surgical procedure")
    ("UMLS:C0751429" "biolink:Procedure" "laparoscopic surgery")
    ("UMLS:C0752217" "biolink:Procedure" "Acupuncture, Ear")
    ("UMLS:C0843593"
     "biolink:Procedure"
     "Laparoscopically Assisted Vaginal Hysterectomy")
    ("UMLS:C0851312" "biolink:IndividualOrganism" "Breast surgery")
    ("UMLS:C0853389" "biolink:Procedure" "Postoperative pain relief")
    ("UMLS:C0886296" "biolink:Procedure" "Nursing interventions")
    ("UMLS:C0920347" "biolink:Procedure" "Procedure on spinal cord")
    ("UMLS:C0937846" "biolink:ChemicalSubstance" "Esomeprazole")
    ("UMLS:C0949216" "biolink:NamedThing" "Complementary Therapies")
    ("UMLS:C0972314" "biolink:NamedThing" "Etoricoxib")
    ("UMLS:C1137094" "biolink:Activity" "Risk Reduction")
    ("UMLS:C1176306" "biolink:ChemicalSubstance" "Aprepitant")
    ("UMLS:C1261322" "biolink:Procedure" "Evaluation - action")
    ("UMLS:C1268547" "biolink:NamedThing" "Patient chart")
    ("UMLS:C1273869" "biolink:Procedure" "Intervention regime")
    ("UMLS:C1327728" "biolink:Procedure" "Operation on female genital organs")
    ("UMLS:C1328580" "biolink:Procedure" "Microvascular Decompression Surgery")
    ("UMLS:C1445610" "biolink:Device" "Acupressure wrist band")
    ("UMLS:C1456587" "biolink:Procedure" "Bariatric Surgery")
    ("UMLS:C1515885" "biolink:Procedure" "Acustimulation Therapy")))

;(test 'all-concept-category-counts
;  (run* (category count)
;    (fresh (curie)
;      (cprop curie "category" category)
;      (:== count (category)
;           (s-length (run^ (curie)
;                           (cprop curie "category" category))))))
;
;  '?)
