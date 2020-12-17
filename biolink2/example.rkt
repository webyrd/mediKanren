#lang racket/base
(require "common.rkt" "db/semmed.rkt" racket/pretty)

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

(test 'single-concept-properties
  (run 10 (k v)
    (cprop "UMLS:C0000137" k v))
  '(("category" "chemical_substance")
    ("id" "UMLS:C0000137")
    ("name" "15S RNA")
    ("umls_type" "(\"T114\" \"T123\")")
    ("umls_type_label"
     "(\"Nucleic Acid, Nucleoside, or Nucleotide\" \"Biologically Active Substance\")")
    ("xrefs" "(\"MESH:D012335\")")))

(test 'chemical-substance-names
  (run 10 (curie name)
    (cprop curie "category" "chemical_substance")
    (cprop curie "name" name))
  '(("UMLS:C0000039" "1,2-dipalmitoylphosphatidylcholine")
    ("UMLS:C0000096" "1-Methyl-3-isobutylxanthine")
    ("UMLS:C0000097" "1-Methyl-4-phenyl-1,2,3,6-tetrahydropyridine")
    ("UMLS:C0000098" "1-Methyl-4-phenylpyridinium")
    ("UMLS:C0000102" "1-Naphthylamine")
    ("UMLS:C0000103" "1-Naphthylisothiocyanate")
    ("UMLS:C0000119" "11-Hydroxycorticosteroids")
    ("UMLS:C0000137" "15S RNA")
    ("UMLS:C0000151" "17 beta-Hydroxy-5 beta-Androstan-3-One")
    ("UMLS:C0000163" "17-Hydroxycorticosteroids")))

(test 'concept-relationships
  (run 10 (curie1 predicate curie2)
    (fresh (eid)
      (eprop eid "edge_label" predicate)
      (edge eid curie1 curie2)))
  '(("UMLS:C1368111" "affects" "UMLS:C1523610")
    ("UMLS:C1414313" "affects" "UMLS:C1523610")
    ("UMLS:C0001962" "affects" "UMLS:C0333440")
    ("UMLS:C0687133" "affects" "UMLS:C1332412")
    ("UMLS:C0687133" "affects" "UMLS:C1332412")
    ("UMLS:C0017262" "affects" "UMLS:C1332412")
    ("UMLS:C0017262" "affects" "UMLS:C1332412")
    ("UMLS:C1522575" "affects" "UMLS:C0002940")
    ("UMLS:C1513016" "affects" "UMLS:C0002940")
    ("UMLS:C1456820" "affects" "UMLS:C0002940")))

(test 'named-negative-regulators
  (run 10 (curie1 name1 curie2 name2)
    (fresh (eid)
      (eprop eid "edge_label" "negatively_regulates")
      (edge eid curie1 curie2)
      (cprop curie1 "name" name1)
      (cprop curie2 "name" name2)))
  '(("UMLS:C1537740" "MIR30D gene" "UMLS:C1332698" "CCNE2 gene")
    ("UMLS:C1537740" "MIR30D gene" "UMLS:C1332698" "CCNE2 gene")
    ("UMLS:C1422386" "HDAC9 gene" "UMLS:C1332698" "CCNE2 gene")
    ("UMLS:C0935996" "Signaling Pathway Gene" "UMLS:C1332698" "CCNE2 gene")
    ("UMLS:C0809183" "CDKN2B gene" "UMLS:C1332698" "CCNE2 gene")
    ("UMLS:C0809183" "CDKN2B gene" "UMLS:C1332698" "CCNE2 gene")
    ("UMLS:C0699251" "Fluimucil" "UMLS:C1332698" "CCNE2 gene")
    ("UMLS:C0288472" "oncoprotein p21" "UMLS:C1332698" "CCNE2 gene")
    ("UMLS:C0288472" "oncoprotein p21" "UMLS:C1332698" "CCNE2 gene")
    ("UMLS:C0286651" "atorvastatin" "UMLS:C1332698" "CCNE2 gene")))

(test 'all-concept-properties
  (run* (k)
    (fresh (curie v)
      (cprop curie k v)))
  '(("category") ("id") ("name") ("umls_type") ("umls_type_label") ("xrefs")))

(test 'all-edge-properties
  (run* (k)
    (fresh (eid v)
      (eprop eid k v)))
  '(("SEMMED_PRED")
    ("edge_label")
    ("is_defined_by")
    ("n_pmids")
    ("negated")
    ("pmids")
    ("provided_by")
    ("relation")))

(test 'all-concept-categories
  (run* (category)
    (fresh (curie)
      (cprop curie "category" category)))
  '(("activity_and_behavior")
  ("anatomical_entity")
  ("biological_entity")
  ("biological_process_or_activity")
  ("cell")
  ("cell_component")
  ("chemical_substance")
  ("disease_or_phenotypic_feature")
  ("gene")
  ("genomic_entity")
  ("gross_anatomical_structure")
  ("phenotypic_feature")
  ("protein")))

(test 'all-predicates
  (run* (predicate)
    (fresh (eid)
      (eprop eid "edge_label" predicate)))
  '(("affects")
  ("causes")
  ("coexists_with")
  ("derives_into")
  ("gene_associated_with_condition")
  ("interacts_with")
  ("location_of")
  ("manifestation_of")
  ("negatively_regulates")
  ("part_of")
  ("positively_regulates")
  ("precedes")
  ("predisposes")
  ("prevents")
  ("produces")
  ("related_to")
  ("subclass_of")
  ("treats")))

(test 'nausea-properties
  (run* (k v)
    (cprop "UMLS:C0520909" k v))
  '(("category" "disease_or_phenotypic_feature")
  ("id" "UMLS:C0520909")
  ("name" "Postoperative Nausea and Vomiting")
  ("umls_type" "(\"T184\")")
  ("umls_type_label" "(\"Sign or Symptom\")")
  ("xrefs"
   "(\"SNMI:F-52846\" \"SNOMEDCT_US:1488000\" \"RCD:Xa0Ka\" \"CHV:0000037809\" \"MESH:D020250\" \"LCH_NW:sh99001642\" \"NDFRT:N0000004075\")")))

(test 'nausea-subject-edges
  (run* (o cat name p)
    (fresh (eid)
      (edge eid "UMLS:C0520909" o)
      (cprop o "category" cat)
      (cprop o "name" name)
      (eprop eid "edge_label" p)))
  '(("UMLS:C0004364"
     "disease_or_phenotypic_feature"
     "Autoimmune Diseases"
     "related_to")
    ("UMLS:C0009566"
     "disease_or_phenotypic_feature"
     "Complication"
     "subclass_of")
    ("UMLS:C0020450"
     "disease_or_phenotypic_feature"
     "Hyperemesis Gravidarum"
     "causes")
    ("UMLS:C0026603"
     "disease_or_phenotypic_feature"
     "Motion Sickness"
     "related_to")
    ("UMLS:C0027497" "disease_or_phenotypic_feature" "Nausea" "subclass_of")
    ("UMLS:C0027497" "disease_or_phenotypic_feature" "Nausea" "causes")
    ("UMLS:C0027497" "disease_or_phenotypic_feature" "Nausea" "coexists_with")
    ("UMLS:C0027498"
     "disease_or_phenotypic_feature"
     "Nausea and vomiting"
     "causes")
    ("UMLS:C0030201"
     "disease_or_phenotypic_feature"
     "Pain, Postoperative"
     "coexists_with")
    ("UMLS:C0032290"
     "disease_or_phenotypic_feature"
     "Aspiration Pneumonia"
     "predisposes")
    ("UMLS:C0032787"
     "disease_or_phenotypic_feature"
     "Postoperative Complications"
     "subclass_of")
    ("UMLS:C0042963" "disease_or_phenotypic_feature" "Vomiting" "subclass_of")
    ("UMLS:C0042963" "disease_or_phenotypic_feature" "Vomiting" "causes")
    ("UMLS:C0042963" "disease_or_phenotypic_feature" "Vomiting" "coexists_with")
    ("UMLS:C0232602" "disease_or_phenotypic_feature" "Retching" "coexists_with")
    ("UMLS:C0234119"
     "disease_or_phenotypic_feature"
     "Neuromuscular inhibition"
     "related_to")
    ("UMLS:C0278134"
     "disease_or_phenotypic_feature"
     "Absence of sensation"
     "causes")
    ("UMLS:C0278134"
     "disease_or_phenotypic_feature"
     "Absence of sensation"
     "coexists_with")
    ("UMLS:C0344307"
     "disease_or_phenotypic_feature"
     "Absence of pain sensation"
     "coexists_with")
    ("UMLS:C0520905"
     "disease_or_phenotypic_feature"
     "Vomiting, Postoperative"
     "coexists_with")
    ("UMLS:C0879626"
     "disease_or_phenotypic_feature"
     "Adverse effects"
     "subclass_of")
    ("UMLS:C1306597"
     "disease_or_phenotypic_feature"
     "Psychiatric problem"
     "manifestation_of")))
