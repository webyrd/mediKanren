CREATE TABLE `alias` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `type` text  NOT NULL
,  `value` varchar(255) NOT NULL
,  `dataset_id` integer NOT NULL
,  CONSTRAINT `fk_alias_dataset` FOREIGN KEY (`dataset_id`) REFERENCES `dataset` (`id`)
,  CONSTRAINT `fk_alias_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `chembl_activity` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `target_id` integer DEFAULT NULL
,  `cmpd_chemblid` varchar(255) NOT NULL
,  `cmpd_name_in_ref` text COLLATE BINARY
,  `smiles` text COLLATE BINARY
,  `act_value` decimal(10,8) DEFAULT NULL
,  `act_type` varchar(255) DEFAULT NULL
,  `reference` text COLLATE BINARY
,  `pubmed_id` integer DEFAULT NULL
,  `cmpd_pubchem_cid` integer DEFAULT NULL
,  CONSTRAINT `fk_chembl_activity__target` FOREIGN KEY (`target_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `compartment` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `ctype` varchar(255) NOT NULL
,  `target_id` integer DEFAULT NULL
,  `protein_id` integer DEFAULT NULL
,  `go_id` varchar(255) DEFAULT NULL
,  `go_term` text COLLATE BINARY
,  `evidence` varchar(255) DEFAULT NULL
,  `zscore` decimal(4,3) DEFAULT NULL
,  `conf` decimal(2,1) DEFAULT NULL
,  `url` text COLLATE BINARY
,  `reliability` text  DEFAULT NULL
,  CONSTRAINT `fk_compartment__compartment_type` FOREIGN KEY (`ctype`) REFERENCES `compartment_type` (`name`)
,  CONSTRAINT `fk_compartment_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_compartment_target` FOREIGN KEY (`target_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `compartment_type` (
  `name` varchar(255) NOT NULL
,  `description` text COLLATE BINARY
,  PRIMARY KEY (`name`)
);
CREATE TABLE `data_type` (
  `name` varchar(7) NOT NULL
,  PRIMARY KEY (`name`)
);
CREATE TABLE `dataset` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `name` varchar(255) NOT NULL
,  `source` text NOT NULL
,  `app` varchar(255) DEFAULT NULL
,  `app_version` varchar(255) DEFAULT NULL
,  `datetime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
,  `url` text COLLATE BINARY
,  `comments` text COLLATE BINARY
);
CREATE TABLE `dbinfo` (
  `dbname` varchar(16) NOT NULL
,  `schema_ver` varchar(16) NOT NULL
,  `data_ver` varchar(16) NOT NULL
,  `owner` varchar(16) NOT NULL
,  `is_copy` integer NOT NULL DEFAULT '0'
,  `dump_file` varchar(64) DEFAULT NULL
);
CREATE TABLE `disease` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `dtype` varchar(255) NOT NULL
,  `target_id` integer DEFAULT NULL
,  `protein_id` integer DEFAULT NULL
,  `name` text NOT NULL
,  `did` varchar(20) DEFAULT NULL
,  `evidence` text COLLATE BINARY
,  `zscore` decimal(4,3) DEFAULT NULL
,  `conf` decimal(2,1) DEFAULT NULL
,  `description` text COLLATE BINARY
,  `reference` varchar(255) DEFAULT NULL
,  `drug_name` text COLLATE BINARY
,  `log2foldchange` decimal(5,3) DEFAULT NULL
,  `pvalue` varchar(255) DEFAULT NULL
,  `score` decimal(16,15) DEFAULT NULL
,  `source` varchar(255) DEFAULT NULL
,  `O2S` decimal(16,13) DEFAULT NULL
,  `S2O` decimal(16,13) DEFAULT NULL
,  CONSTRAINT `fk_disease__disease_type` FOREIGN KEY (`dtype`) REFERENCES `disease_type` (`name`)
,  CONSTRAINT `fk_disease_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_disease_target` FOREIGN KEY (`target_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `disease_type` (
  `name` varchar(255) NOT NULL
,  `description` text COLLATE BINARY
,  PRIMARY KEY (`name`)
);
CREATE TABLE `do` (
  `id` varchar(255) NOT NULL
,  `name` text NOT NULL
,  `def` text COLLATE BINARY
,  PRIMARY KEY (`id`)
);
CREATE TABLE `do_parent` (
  `doid` varchar(255) NOT NULL
,  `parent` varchar(255) DEFAULT NULL
);
CREATE TABLE `drug_activity` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `target_id` integer NOT NULL
,  `drug` varchar(255) NOT NULL
,  `act_value` decimal(10,8) DEFAULT NULL
,  `act_type` varchar(255) DEFAULT NULL
,  `action_type` varchar(255) DEFAULT NULL
,  `has_moa` integer NOT NULL
,  `source` varchar(255) DEFAULT NULL
,  `reference` text COLLATE BINARY
,  `smiles` text COLLATE BINARY
,  `cmpd_chemblid` varchar(255) DEFAULT NULL
,  `nlm_drug_info` text COLLATE BINARY
,  `cmpd_pubchem_cid` integer DEFAULT NULL
,  CONSTRAINT `fk_drug_activity__target` FOREIGN KEY (`target_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `dto` (
  `id` varchar(255) NOT NULL
,  `name` text NOT NULL
,  `parent` varchar(255) DEFAULT NULL
,  PRIMARY KEY (`id`)
,  CONSTRAINT `fk_dto_dto` FOREIGN KEY (`parent`) REFERENCES `dto` (`id`)
);
CREATE TABLE `expression` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `etype` varchar(255) NOT NULL
,  `target_id` integer DEFAULT NULL
,  `protein_id` integer DEFAULT NULL
,  `tissue` text NOT NULL
,  `qual_value` text  DEFAULT NULL
,  `number_value` decimal(12,6) DEFAULT NULL
,  `boolean_value` integer DEFAULT NULL
,  `string_value` text COLLATE BINARY
,  `pubmed_id` integer DEFAULT NULL
,  `evidence` varchar(255) DEFAULT NULL
,  `zscore` decimal(4,3) DEFAULT NULL
,  `conf` decimal(2,1) DEFAULT NULL
,  `oid` varchar(20) DEFAULT NULL
,  `confidence` integer DEFAULT NULL
,  `age` varchar(20) DEFAULT NULL
,  `gender` varchar(20) DEFAULT NULL
,  `url` text COLLATE BINARY
,  CONSTRAINT `fk_expression__expression_type` FOREIGN KEY (`etype`) REFERENCES `expression_type` (`name`)
,  CONSTRAINT `fk_expression__protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_expression__target` FOREIGN KEY (`target_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `expression_type` (
  `name` varchar(255) NOT NULL
,  `data_type` varchar(7) NOT NULL
,  `description` text COLLATE BINARY
,  PRIMARY KEY (`name`)
,  UNIQUE (`name`,`data_type`)
,  CONSTRAINT `fk_expression_type__data_type` FOREIGN KEY (`data_type`) REFERENCES `data_type` (`name`)
);
CREATE TABLE `feature` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `type` varchar(255) NOT NULL
,  `description` text COLLATE BINARY
,  `srcid` varchar(255) DEFAULT NULL
,  `evidence` varchar(255) DEFAULT NULL
,  `begin` integer DEFAULT NULL
,  `end` integer DEFAULT NULL
,  `position` integer DEFAULT NULL
,  `original` varchar(255) DEFAULT NULL
,  `variation` varchar(255) DEFAULT NULL
,  CONSTRAINT `fk_feature_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `gene_attribute` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `gat_id` integer NOT NULL
,  `name` text NOT NULL
,  `value` integer NOT NULL
,  CONSTRAINT `fk_gene_attribute__gene_attribute_type` FOREIGN KEY (`gat_id`) REFERENCES `gene_attribute_type` (`id`)
,  CONSTRAINT `fk_gene_attribute__protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `gene_attribute_type` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `name` varchar(255) NOT NULL
,  `association` text NOT NULL
,  `description` text NOT NULL
,  `resource_group` text  NOT NULL
,  `measurement` varchar(255) NOT NULL
,  `attribute_group` varchar(255) NOT NULL
,  `attribute_type` varchar(255) NOT NULL
,  `pubmed_ids` text COLLATE BINARY
,  `url` text COLLATE BINARY
,  UNIQUE (`name`)
);
CREATE TABLE `generif` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `pubmed_ids` text COLLATE BINARY
,  `text` text NOT NULL
,  `years` text COLLATE BINARY
,  CONSTRAINT `fk_generif_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `goa` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `go_id` varchar(255) NOT NULL
,  `go_term` text COLLATE BINARY
,  `evidence` text COLLATE BINARY
,  `goeco` varchar(255) NOT NULL
,  CONSTRAINT `fk_goa_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `grant` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `target_id` integer DEFAULT NULL
,  `protein_id` integer DEFAULT NULL
,  `appid` integer NOT NULL
,  `full_project_num` varchar(255) NOT NULL
,  `activity` varchar(4) NOT NULL
,  `funding_ics` varchar(255) NOT NULL
,  `year` integer NOT NULL
,  `cost` decimal(12,2) NOT NULL
,  CONSTRAINT `fk_grant_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_grant_target` FOREIGN KEY (`target_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `hgram_cdf` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `type` varchar(255) NOT NULL
,  `attr_count` integer NOT NULL
,  `attr_cdf` decimal(17,16) NOT NULL
,  CONSTRAINT `fk_hgram_cdf__gene_attribute_type` FOREIGN KEY (`type`) REFERENCES `gene_attribute_type` (`name`)
,  CONSTRAINT `fk_hgram_cdf__protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `info_type` (
  `name` varchar(255) NOT NULL
,  `data_type` varchar(7) NOT NULL
,  `unit` varchar(255) DEFAULT NULL
,  `description` text COLLATE BINARY
,  PRIMARY KEY (`name`)
,  UNIQUE (`name`,`data_type`)
,  UNIQUE (`name`,`data_type`)
,  CONSTRAINT `fk_info_type__data_type` FOREIGN KEY (`data_type`) REFERENCES `data_type` (`name`)
);
CREATE TABLE `kegg_distance` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `pid1` integer NOT NULL
,  `pid2` integer NOT NULL
,  `distance` integer NOT NULL
,  CONSTRAINT `fk_kegg_distance__protein1` FOREIGN KEY (`pid1`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_kegg_distance__protein2` FOREIGN KEY (`pid2`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `kegg_nearest_tclin` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `tclin_id` integer NOT NULL
,  `direction` text  NOT NULL
,  `distance` integer NOT NULL
,  CONSTRAINT `fk_kegg_nearest_tclin__protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_kegg_nearest_tclin__target` FOREIGN KEY (`tclin_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `locsig` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `location` varchar(255) NOT NULL
,  `signal` varchar(255) NOT NULL
,  `pmids` text COLLATE BINARY
,  CONSTRAINT `fk_locsig_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `mlp_assay_info` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `assay_name` text NOT NULL
,  `method` varchar(255) NOT NULL
,  `active_sids` integer DEFAULT NULL
,  `inactive_sids` integer DEFAULT NULL
,  `iconclusive_sids` integer DEFAULT NULL
,  `total_sids` integer DEFAULT NULL
,  `aid` integer DEFAULT NULL
,  CONSTRAINT `fk_mai_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `ortholog` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `taxid` integer NOT NULL
,  `species` varchar(255) NOT NULL
,  `db_id` varchar(255) DEFAULT NULL
,  `geneid` integer DEFAULT NULL
,  `symbol` varchar(255) NOT NULL
,  `name` varchar(255) NOT NULL
,  `mod_url` text COLLATE BINARY
,  `sources` varchar(255) NOT NULL
,  CONSTRAINT `fk_ortholog_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `p2pc` (
  `panther_class_id` integer NOT NULL
,  `protein_id` integer NOT NULL
,  CONSTRAINT `fk_p2pc__panther_class` FOREIGN KEY (`panther_class_id`) REFERENCES `panther_class` (`id`)
,  CONSTRAINT `fk_p2pc_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `panther_class` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `pcid` char(7) NOT NULL
,  `parent_pcids` varchar(255) DEFAULT NULL
,  `name` text NOT NULL
,  `description` text COLLATE BINARY
,  UNIQUE (`pcid`)
);
CREATE TABLE `patent_count` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `year` integer NOT NULL
,  `count` integer NOT NULL
,  CONSTRAINT `fk_patent_count__protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `pathway` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `target_id` integer DEFAULT NULL
,  `protein_id` integer DEFAULT NULL
,  `pwtype` varchar(255) NOT NULL
,  `id_in_source` varchar(255) DEFAULT NULL
,  `name` text NOT NULL
,  `description` text COLLATE BINARY
,  `url` text COLLATE BINARY
,  CONSTRAINT `fk_pathway__pathway_type` FOREIGN KEY (`pwtype`) REFERENCES `pathway_type` (`name`)
,  CONSTRAINT `fk_pathway_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_pathway_target` FOREIGN KEY (`target_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `pathway_type` (
  `name` varchar(255) NOT NULL
,  `url` text COLLATE BINARY
,  PRIMARY KEY (`name`)
);
CREATE TABLE `phenotype` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `ptype` varchar(255) NOT NULL
,  `target_id` integer DEFAULT NULL
,  `protein_id` integer DEFAULT NULL
,  `trait` text COLLATE BINARY
,  `pmid` integer DEFAULT NULL
,  `snps` varchar(255) DEFAULT NULL
,  `top_level_term_id` varchar(255) DEFAULT NULL
,  `top_level_term_name` varchar(255) DEFAULT NULL
,  `term_id` varchar(255) DEFAULT NULL
,  `term_name` varchar(255) DEFAULT NULL
,  `p_value` decimal(20,19) DEFAULT NULL
,  `percentage_change` varchar(255) DEFAULT NULL
,  `effect_size` varchar(255) DEFAULT NULL
,  `statistical_method` text COLLATE BINARY
,  `term_description` text COLLATE BINARY
,  CONSTRAINT `fk_phenotype_info_type` FOREIGN KEY (`ptype`) REFERENCES `phenotype_type` (`name`)
,  CONSTRAINT `fk_phenotype_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_phenotype_target` FOREIGN KEY (`target_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `phenotype_type` (
  `name` varchar(255) NOT NULL
,  `ontology` varchar(255) DEFAULT NULL
,  `description` text COLLATE BINARY
,  PRIMARY KEY (`name`)
,  UNIQUE (`name`,`ontology`)
);
CREATE TABLE `pmscore` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `year` integer NOT NULL
,  `score` decimal(12,6) NOT NULL
,  CONSTRAINT `fk_pmscore_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `ppi` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `ppitype` varchar(255) NOT NULL
,  `protein1_id` integer NOT NULL
,  `protein1_str` varchar(255) DEFAULT NULL
,  `protein2_id` integer NOT NULL
,  `protein2_str` varchar(255) DEFAULT NULL
,  `p_int` decimal(10,9) DEFAULT NULL
,  `p_ni` decimal(10,9) DEFAULT NULL
,  `p_wrong` decimal(10,9) DEFAULT NULL
,  `evidence` varchar(255) DEFAULT NULL
,  CONSTRAINT `fk_ppi_protein1` FOREIGN KEY (`protein1_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_ppi_protein2` FOREIGN KEY (`protein2_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `ppi_type` (
  `name` varchar(255) NOT NULL
,  `description` text COLLATE BINARY
,  `url` text COLLATE BINARY
,  PRIMARY KEY (`name`)
);
CREATE TABLE `protein` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `name` varchar(255) NOT NULL
,  `description` text NOT NULL
,  `uniprot` varchar(20) NOT NULL
,  `up_version` integer DEFAULT NULL
,  `geneid` integer DEFAULT NULL
,  `sym` varchar(20) DEFAULT NULL
,  `family` varchar(255) DEFAULT NULL
,  `chr` varchar(255) DEFAULT NULL
,  `seq` text COLLATE BINARY
,  `dtoid` varchar(13) DEFAULT NULL
,  `stringid` varchar(15) DEFAULT NULL
,  UNIQUE (`uniprot`)
,  UNIQUE (`name`)
);
CREATE TABLE `protein2pubmed` (
  `protein_id` integer NOT NULL
,  `pubmed_id` integer NOT NULL
,  CONSTRAINT `fk_protein2pubmed__protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_protein2pubmed__pubmed` FOREIGN KEY (`pubmed_id`) REFERENCES `pubmed` (`id`)
);
CREATE TABLE `provenance` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `dataset_id` integer NOT NULL
,  `table_name` varchar(255) NOT NULL
,  `column_name` varchar(255) DEFAULT NULL
,  `where_clause` text COLLATE BINARY
,  `comment` text COLLATE BINARY
,  CONSTRAINT `fk_provenance_dataset` FOREIGN KEY (`dataset_id`) REFERENCES `dataset` (`id`)
);
CREATE TABLE `ptscore` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `year` integer NOT NULL
,  `score` decimal(12,6) NOT NULL
,  CONSTRAINT `fk_ptscore_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `pubmed` (
  `id` integer NOT NULL
,  `title` text NOT NULL
,  `journal` text COLLATE BINARY
,  `date` varchar(10) DEFAULT NULL
,  `authors` text COLLATE BINARY
,  `abstract` text COLLATE BINARY
,  PRIMARY KEY (`id`)
);
CREATE TABLE `t2tc` (
  `target_id` integer NOT NULL
,  `protein_id` integer DEFAULT NULL
,  `nucleic_acid_id` integer DEFAULT NULL
,  CONSTRAINT `fk_t2tc__protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`)
,  CONSTRAINT `fk_t2tc__target` FOREIGN KEY (`target_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `target` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `name` varchar(255) NOT NULL
,  `ttype` varchar(255) NOT NULL
,  `description` text COLLATE BINARY
,  `comment` text COLLATE BINARY
,  `tdl` text  DEFAULT NULL
,  `idg2` integer NOT NULL DEFAULT '0'
,  `fam` text  DEFAULT NULL
,  `famext` varchar(255) DEFAULT NULL
);
CREATE TABLE `tdl_info` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `itype` varchar(255) NOT NULL
,  `target_id` integer DEFAULT NULL
,  `protein_id` integer DEFAULT NULL
,  `nucleic_acid_id` integer DEFAULT NULL
,  `string_value` text COLLATE BINARY
,  `number_value` decimal(12,6) DEFAULT NULL
,  `integer_value` integer DEFAULT NULL
,  `date_value` date DEFAULT NULL
,  `boolean_value` integer DEFAULT NULL
,  `curration_level` varchar(50) DEFAULT NULL
,  CONSTRAINT `fk_tdl_info__info_type` FOREIGN KEY (`itype`) REFERENCES `info_type` (`name`)
,  CONSTRAINT `fk_tdl_info__protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_tdl_info__target` FOREIGN KEY (`target_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `tdl_update_log` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `target_id` integer NOT NULL
,  `old_tdl` varchar(10) DEFAULT NULL
,  `new_tdl` varchar(10) NOT NULL
,  `person` varchar(255) NOT NULL
,  `datetime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
,  `explanation` text COLLATE BINARY
,  `application` varchar(255) DEFAULT NULL
,  `app_version` varchar(255) DEFAULT NULL
,  CONSTRAINT `fk_tdl_update_log__target` FOREIGN KEY (`target_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `techdev_contact` (
  `id` integer NOT NULL
,  `contact_name` varchar(255) NOT NULL
,  `contact_email` varchar(255) NOT NULL
,  `date` date DEFAULT NULL
,  `grant_number` varchar(255) DEFAULT NULL
,  `pi` varchar(255) NOT NULL
,  PRIMARY KEY (`id`)
);
CREATE TABLE `techdev_info` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `contact_id` integer DEFAULT NULL
,  `protein_id` integer DEFAULT NULL
,  `comment` text NOT NULL
,  `publication_pcmid` varchar(255) DEFAULT NULL
,  `publication_pmid` integer DEFAULT NULL
,  `resource_url` text COLLATE BINARY
,  `data_url` text COLLATE BINARY
,  CONSTRAINT `fk_techdev_info__protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_techdev_info__techdev_contact` FOREIGN KEY (`contact_id`) REFERENCES `techdev_contact` (`id`)
);
CREATE TABLE `tinx_articlerank` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `importance_id` integer NOT NULL
,  `pmid` integer NOT NULL
,  `rank` integer NOT NULL
,  CONSTRAINT `fk_tinx_articlerank__tinx_importance` FOREIGN KEY (`importance_id`) REFERENCES `tinx_importance` (`id`) ON DELETE CASCADE
);
CREATE TABLE `tinx_disease` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `doid` varchar(20) NOT NULL
,  `name` text NOT NULL
,  `summary` text COLLATE BINARY
,  `score` decimal(34,16) DEFAULT NULL
);
CREATE TABLE `tinx_importance` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `disease_id` integer NOT NULL
,  `score` decimal(34,16) NOT NULL
,  CONSTRAINT `fk_tinx_importance__protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_tinx_importance__tinx_disease` FOREIGN KEY (`disease_id`) REFERENCES `tinx_disease` (`id`)
);
CREATE TABLE `tinx_novelty` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `protein_id` integer NOT NULL
,  `score` decimal(34,16) NOT NULL
,  CONSTRAINT `fk_tinx_novelty__protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
);
CREATE TABLE `xref` (
  `id` integer NOT NULL PRIMARY KEY AUTOINCREMENT
,  `xtype` varchar(255) NOT NULL
,  `target_id` integer DEFAULT NULL
,  `protein_id` integer DEFAULT NULL
,  `nucleic_acid_id` integer DEFAULT NULL
,  `value` varchar(255) NOT NULL
,  `xtra` varchar(255) DEFAULT NULL
,  `dataset_id` integer NOT NULL
,  UNIQUE (`xtype`,`target_id`,`value`)
,  UNIQUE (`xtype`,`protein_id`,`value`)
,  CONSTRAINT `fk_xref__xref_type` FOREIGN KEY (`xtype`) REFERENCES `xref_type` (`name`)
,  CONSTRAINT `fk_xref_dataset` FOREIGN KEY (`dataset_id`) REFERENCES `dataset` (`id`)
,  CONSTRAINT `fk_xref_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`id`) ON DELETE CASCADE
,  CONSTRAINT `fk_xref_target` FOREIGN KEY (`target_id`) REFERENCES `target` (`id`) ON DELETE CASCADE
);
CREATE TABLE `xref_type` (
  `name` varchar(255) NOT NULL
,  `description` text COLLATE BINARY
,  `url` text COLLATE BINARY
,  `eg_q_url` text COLLATE BINARY
,  PRIMARY KEY (`name`)
);
CREATE INDEX "idx_tinx_articlerank_tinx_articlerank_idx1" ON "tinx_articlerank" (`importance_id`);
CREATE INDEX "idx_t2tc_t2tc_idx1" ON "t2tc" (`target_id`);
CREATE INDEX "idx_t2tc_t2tc_idx2" ON "t2tc" (`protein_id`);
CREATE INDEX "idx_info_type_fk_info_type__data_type" ON "info_type" (`data_type`);
CREATE INDEX "idx_pmscore_pmscore_idx1" ON "pmscore" (`protein_id`);
CREATE INDEX "idx_xref_xref_idx1" ON "xref" (`xtype`);
CREATE INDEX "idx_xref_xref_idx2" ON "xref" (`target_id`);
CREATE INDEX "idx_xref_xref_idx4" ON "xref" (`protein_id`);
CREATE INDEX "idx_xref_xref_idx6" ON "xref" (`dataset_id`);
CREATE INDEX "idx_dto_dto_idx1" ON "dto" (`parent`);
CREATE INDEX "idx_ppi_ppi_idx1" ON "ppi" (`protein1_id`);
CREATE INDEX "idx_ppi_ppi_idx2" ON "ppi" (`protein2_id`);
CREATE INDEX "idx_ppi_ppi_idx3" ON "ppi" (`ppitype`);
CREATE INDEX "idx_ptscore_ptscore_idx1" ON "ptscore" (`protein_id`);
CREATE INDEX "idx_drug_activity_drug_activity_idx1" ON "drug_activity" (`target_id`);
CREATE INDEX "idx_tinx_importance_tinx_importance_idx1" ON "tinx_importance" (`protein_id`);
CREATE INDEX "idx_tinx_importance_tinx_importance_idx2" ON "tinx_importance" (`disease_id`);
CREATE INDEX "idx_gene_attribute_gene_attribute_idx1" ON "gene_attribute" (`protein_id`);
CREATE INDEX "idx_gene_attribute_gene_attribute_idx2" ON "gene_attribute" (`gat_id`);
CREATE INDEX "idx_feature_feature_idx1" ON "feature" (`protein_id`);
CREATE INDEX "idx_compartment_compartment_idx1" ON "compartment" (`ctype`);
CREATE INDEX "idx_compartment_compartment_idx2" ON "compartment" (`target_id`);
CREATE INDEX "idx_compartment_compartment_idx3" ON "compartment" (`protein_id`);
CREATE INDEX "idx_p2pc_p2pc_idx1" ON "p2pc" (`panther_class_id`);
CREATE INDEX "idx_p2pc_p2pc_idx2" ON "p2pc" (`protein_id`);
CREATE INDEX "idx_chembl_activity_chembl_activity_idx1" ON "chembl_activity" (`target_id`);
CREATE INDEX "idx_locsig_compartment_idx1" ON "locsig" (`protein_id`);
CREATE INDEX "idx_goa_goa_idx1" ON "goa" (`protein_id`);
CREATE INDEX "idx_tdl_info_tdl_info_idx1" ON "tdl_info" (`itype`);
CREATE INDEX "idx_tdl_info_tdl_info_idx2" ON "tdl_info" (`target_id`);
CREATE INDEX "idx_tdl_info_tdl_info_idx3" ON "tdl_info" (`protein_id`);
CREATE INDEX "idx_protein2pubmed_protein2pubmed_idx1" ON "protein2pubmed" (`protein_id`);
CREATE INDEX "idx_protein2pubmed_protein2pubmed_idx2" ON "protein2pubmed" (`pubmed_id`);
CREATE INDEX "idx_ortholog_ortholog_idx1" ON "ortholog" (`protein_id`);
CREATE INDEX "idx_disease_disease_idx1" ON "disease" (`dtype`);
CREATE INDEX "idx_disease_disease_idx2" ON "disease" (`target_id`);
CREATE INDEX "idx_disease_disease_idx3" ON "disease" (`protein_id`);
CREATE INDEX "idx_techdev_info_techdev_info_idx1" ON "techdev_info" (`contact_id`);
CREATE INDEX "idx_techdev_info_techdev_info_idx2" ON "techdev_info" (`protein_id`);
CREATE INDEX "idx_phenotype_phenotype_idx1" ON "phenotype" (`ptype`);
CREATE INDEX "idx_phenotype_phenotype_idx2" ON "phenotype" (`target_id`);
CREATE INDEX "idx_phenotype_phenotype_idx3" ON "phenotype" (`protein_id`);
CREATE INDEX "idx_mlp_assay_info_mlp_assay_info_idx1" ON "mlp_assay_info" (`protein_id`);
CREATE INDEX "idx_grant_grant_idx1" ON "grant" (`target_id`);
CREATE INDEX "idx_grant_grant_idx2" ON "grant" (`protein_id`);
CREATE INDEX "idx_tdl_update_log_tdl_update_log" ON "tdl_update_log" (`target_id`);
CREATE INDEX "idx_patent_count_patent_count_idx1" ON "patent_count" (`protein_id`);
CREATE INDEX "idx_expression_expression_idx1" ON "expression" (`etype`);
CREATE INDEX "idx_expression_expression_idx2" ON "expression" (`target_id`);
CREATE INDEX "idx_expression_expression_idx3" ON "expression" (`protein_id`);
CREATE INDEX "idx_tinx_novelty_tinx_novelty_idx1" ON "tinx_novelty" (`protein_id`);
CREATE INDEX "idx_provenance_provenance_idx1" ON "provenance" (`dataset_id`);
CREATE INDEX "idx_kegg_distance_kegg_distance_idx1" ON "kegg_distance" (`pid1`);
CREATE INDEX "idx_kegg_distance_kegg_distance_idx2" ON "kegg_distance" (`pid2`);
CREATE INDEX "idx_generif_generif_idx1" ON "generif" (`protein_id`);
CREATE INDEX "idx_expression_type_fk_expression_type__data_type" ON "expression_type" (`data_type`);
CREATE INDEX "idx_alias_alias_idx1" ON "alias" (`protein_id`);
CREATE INDEX "idx_alias_alias_idx2" ON "alias" (`dataset_id`);
CREATE INDEX "idx_kegg_nearest_tclin_kegg_nearest_tclin_idx1" ON "kegg_nearest_tclin" (`protein_id`);
CREATE INDEX "idx_kegg_nearest_tclin_kegg_nearest_tclin_idx2" ON "kegg_nearest_tclin" (`tclin_id`);
CREATE INDEX "idx_hgram_cdf_hgram_cdf_idx1" ON "hgram_cdf" (`protein_id`);
CREATE INDEX "idx_hgram_cdf_hgram_cdf_idx2" ON "hgram_cdf" (`type`);
CREATE INDEX "idx_pathway_pathway_idx1" ON "pathway" (`target_id`);
CREATE INDEX "idx_pathway_pathway_idx2" ON "pathway" (`protein_id`);
CREATE INDEX "idx_pathway_pathway_idx3" ON "pathway" (`pwtype`);
