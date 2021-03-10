- edge 
  :ID   :START                               :END
  0     '<molecule_dictionary_CHEMBL_ID>'    '<target_dictionaryCHEMBL_ID>'
  1     ...                                  ... 

- edgeprops 
  :ID   propname                  value
  0     edge_label                '<relationship_typeRELATIONSHIP_DESC>'
  0     subject_curie             '<molecule_dictionary_CHEMBL_ID>'
  0     subject_name              {'string_names' : {'preferred_chemical_name' : '<molecule_dictionary_PREFNAME>' }
                                                     'IUPAC_chemical_name' : '<compound_recordsCOMPOUND_NAME>' }
  0     subject_curie_synonyms    {...?curie-synonyms?...}
  0     subject_ontology_parent   {'cross_mapped_ontology_parent' : {CHEBI_parent : <molecule_dictionaryCHEBI_PAR_ID>}}>
  0     object_curie              '<target_dictionaryCHEMBL_ID>'
  0     object_name               '<target_dictionaryPREF_NAME>'
  0     object_ontology_parent    {'cross_mapped_ontology_parent' : {...?UniProtKB:ProteinFamily?...}}>
  0     object_curie_synonyms     {...?curie-synonyms?...}
  0     simplified_edge_label     '<...?biolink-predicate?...>'
  0     experimental_evidence     {'CHEMBL_27': {'assay_id' : '<activitiesASSAY_ID>'},
                                                {'assay_publication_info' : {'assay_PMID' : '<docs_for_assayPUBMED_ID>',
                                                                             'assay_DOI' : '<docs_for_assayDOI>',
                                                                             'assay_description' : '<assaysDECRIPTION>',
                                                                             'assay_curation_method' : '<assaysCURATED_BY>',
                                                                             'assay_confidence_score' : '<assaysCONFIDENCE_SCORE>',
                                                                             'assay_info_last_update' : '<assaysUPDATED_ON>',
                                                                             'assay_publication_date' : '<docs_for_assayYEAR>',
                                                                             'assay_publication_journal' : '<docs_for_assayJOURNAL>',
                                                                             'assay_publication_title' : '<docs_for_assayTITLE>', 
                                                                             'assay_publication_authors' : '<docs_for_assayAUTHORS>',
                                                                             'assay_publication_abstract' : '<docs_for_assayABSTRACT>'}},
                                                {'assay_paramater_info' : {'assay_target_type' : '<target_dictionaryTARGET_TYPE>',
                                                                           'assay_target_chembl_id' : '<target_dictionaryCHEMBL_ID>',
                                                                           'assay_target_chembl_name' : '<target_dictionaryPREF_NAME>',
                                                                           'assay_type' : '<assay_typeASSAY_DESC>',
                                                                           'assay_organism' : '<assaysASSAY_ORGANISM>',
                                                                           'assay_organism_taxon_id' : '<assaysASSAY_TAX_ID>',
                                                                           'assay_tissue_type' : '<assayASSAY_TISSUE>',
                                                                           'assay_tissue_type_curies' : '<target_dictionaryUBERON_ID>',
                                                                           'assay_cell_type' : '<assaysASSAY_CELL_TYPE>',
                                                                           'assay_cell_taxon' : {'<cell_dictionaryCELL_SOURCE_TAX_ID>'}
                                                                           'assay_cell_type_curies' : {'<cell_dictionaryCLO_ID>',
                                                                                                       '<cell_dictionaryEFO_ID>',
                                                                                                       '<cell_dictionaryCELLOSAURUS_ID>',
                                                                                                       '<cell_dictionaryCL_LINCS_ID>',
                                                                                                       '<cell_dictionaryCELL_ONTOLOGY_ID>'}}},
                                                {'assay_experimental_data' : {'assay_measurement_type' : '<activitiesSTANDARD_TYPE>',
                                                                              'assay_measurment_relation' : '<activitiesSTANDARD_RELATION>'
                                                                              'assay_measurment_value' : '<activitiesSTANDARD_VALUE>'
                                                                              'assay_measurement_units' : '<activitiesSTANDARD_UNITS>',
                                                                              'assay_chembl_pvalue' : '<activitiesPCHEMBL_VALUE>',
                                                                              'assay_measurement_type_BAO_name' : '<bioassay_ontologyLABEL>'
                                                                              'assay_measurement_type_BAO_curie' : '<activitiesBAO_END>'
                                                                              'assay_measurement_units_UO_curie' : '<activitiesUO_UNITS>'
                                                                              'assay_measurement_units_QUDT_id' : '<activitiesQUDT_UNITS>'}}}
