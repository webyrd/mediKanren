#lang racket/base
(require "../common.rkt" "../db/semmed.rkt" racket/pretty)

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

(test 'various-edge-properties
  (run 20 (id k v) (eprop id k v))
  '((0 "SEMMED_PRED" "AFFECTS")
    (1 "SEMMED_PRED" "AFFECTS")
    (119 "SEMMED_PRED" "AFFECTS")
    (502 "SEMMED_PRED" "AFFECTS")
    (503 "SEMMED_PRED" "AFFECTS")
    (504 "SEMMED_PRED" "AFFECTS")
    (505 "SEMMED_PRED" "AFFECTS")
    (2639 "SEMMED_PRED" "AFFECTS")
    (2641 "SEMMED_PRED" "AFFECTS")
    (2645 "SEMMED_PRED" "AFFECTS")
    (2648 "SEMMED_PRED" "AFFECTS")
    (2649 "SEMMED_PRED" "AFFECTS")
    (2651 "SEMMED_PRED" "AFFECTS")
    (2652 "SEMMED_PRED" "AFFECTS")
    (2653 "SEMMED_PRED" "AFFECTS")
    (2654 "SEMMED_PRED" "AFFECTS")
    (2657 "SEMMED_PRED" "AFFECTS")
    (2659 "SEMMED_PRED" "AFFECTS")
    (2660 "SEMMED_PRED" "AFFECTS")
    (2661 "SEMMED_PRED" "AFFECTS")))

(test 'various-names
  (run 17 (c n) (cprop c "name" n))
  '(("UMLS:C1156332" "'de novo' GDP-L-fucose biosynthetic process")
    ("UMLS:C1157653" "'de novo' IMP biosynthetic process")
    ("UMLS:C1523107" "'de novo' cotranslational protein folding")
    ("UMLS:C1158942" "'de novo' protein folding")
    ("UMLS:C0761898" "(((4-chloromethyl)benzoyl)amino)-tetramethylrhodamine")
    ("UMLS:C1260101"
     "(((4-nitrophenyl)amino)(2,2,4,4-tetramethyl thiochroman-6-yl)amino) methane-1-thione")
    ("UMLS:C0643014" "((2-azido-4-benzyl)phenoxy)-N-ethylmorpholine")
    ("UMLS:C0635089"
     "((3-(bis(2-chloroethyl)amino)-4-methylphenyl)hydroxymethane)bisphosphonic acid")
    ("UMLS:C1312836"
     "((3Z)-N-(3-chlorophenyl)-3-((3,5-dimethyl-4-((4-methylpiperazin-1-yl)carbonyl)-1H-pyrrol-2-yl)methylene)-N-methyl-2-oxo-2,3-dihydro-1H-indole-5-sulfonamide)")
    ("UMLS:C0378071"
     "((5,6-dichloro-2,3,9,9a-tetrahydro-3-oxo-9a-propyl-1H-fluoren-7-yl)oxy)acetic acid")
    ("UMLS:C0961234"
     "((6-acetyl-3,4-dihydro-2,5,7,8-tetramethyl-2H-1-benzopyran-2yl)carbonyl)-3(aminoethyl)indole")
    ("UMLS:C0915106"
     "((E)-(5)-(3,5-di-tert-butyl-4-hydroxybenzylidene)- 2-ethyl-1,2-isothiazolidine-1,1-dioxide)")
    ("UMLS:C1565913"
     "((S)-2-acetylamino-3-((R)-(1-(3-(piperidin-4-yl)propionyl)piperidin-3-ylcarbonyl))amino) propionic acid trihydrate")
    ("UMLS:C0043560" "((dihydroindenyl)oxy)alkanoic acid")
    ("UMLS:C0967118" "((n-nitroveratryl)oxy)chlorocarbamate-caged thymosin beta4")
    ("UMLS:C0257864" "((nitroveratryl)oxy)chlorocarbamate")
    ("UMLS:C1158357" "(+)-camphor metabolic process")))

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

(test 'single-subject-edges
  (run 20 (id o) (edge id "UMLS:C0005767" o))
  '((11582417 "UMLS:C0000039")
    (4175774 "UMLS:C0000084")
    (3396436 "UMLS:C0000103")
    (3475411 "UMLS:C0000119")
    (9750416 "UMLS:C0000163")
    (6336638 "UMLS:C0000165")
    (7906789 "UMLS:C0000167")
    (1604793 "UMLS:C0000173")
    (9276373 "UMLS:C0000215")
    (2443460 "UMLS:C0000248")
    (11125423 "UMLS:C0000257")
    (5177191 "UMLS:C0000266")
    (12962849 "UMLS:C0000294")
    (769969 "UMLS:C0000343")
    (6609976 "UMLS:C0000376")
    (6784014 "UMLS:C0000379")
    (448326 "UMLS:C0000392")
    (1110409 "UMLS:C0000407")
    (13525868 "UMLS:C0000473")
    (10823557 "UMLS:C0000477")))

(test 'single-object-edges
  (run 20 (id s) (edge id s "UMLS:C0005767"))
  '((6463675 "UMLS:C0000119")
    (6463674 "UMLS:C0000163")
    (6466559 "UMLS:C0000163")
    (6466558 "UMLS:C0000167")
    (6463673 "UMLS:C0000340")
    (6466556 "UMLS:C0000379")
    (6466557 "UMLS:C0000379")
    (6463672 "UMLS:C0000392")
    (6463671 "UMLS:C0000464")
    (6466555 "UMLS:C0000503")
    (6463670 "UMLS:C0000530")
    (6466554 "UMLS:C0000530")
    (6463669 "UMLS:C0000545")
    (6466553 "UMLS:C0000545")
    (6463668 "UMLS:C0000578")
    (6463667 "UMLS:C0000608")
    (6463666 "UMLS:C0000617")
    (6466551 "UMLS:C0000617")
    (6466552 "UMLS:C0000617")
    (6463665 "UMLS:C0000779")))

(test 'single-edge
  (run* (id) (edge id "UMLS:C0005767" "UMLS:C1121571"))
  '((10012443)))

(test 'single-edge-properties
  (run* (id k v)
    (edge id "UMLS:C0005767" "UMLS:C1121571")
    (eprop id k v))
  '((10012443 "SEMMED_PRED" "LOCATION_OF")
    (10012443 "edge_label" "location_of")
    (10012443 "is_defined_by" "semmeddb")
    (10012443 "n_pmids" "14")
    (10012443 "negated" "False")
    (10012443
     "pmids"
     "19402156;21663591;23551806;23551806;23551806;23551806;27062891;27253267;24048877;28903084;17297611;22683313;22172068;26418915")
    (10012443 "provided_by" "semmeddb_sulab")
    (10012443 "relation" "semmeddb:location_of")))

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

(test 'gene-names
  (run 10 (c n)
    (cprop c "name"     n)
    (cprop c "category" "gene"))
  '(("UMLS:C0002085" "Alleles")
    ("UMLS:C0008288" "CIPC gene")
    ("UMLS:C0008844" "Cistron")
    ("UMLS:C0017258" "Gene Clusters")
    ("UMLS:C0017272" "Gene Library")
    ("UMLS:C0017337" "Genes")
    ("UMLS:C0017338" "Genes, araC")
    ("UMLS:C0017339" "Genes, Bacterial")
    ("UMLS:C0017340" "Genes, Developmental")
    ("UMLS:C0017342" "Genes, Dominant")))

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

;; ~20 seconds
;(test 'single-object-affects-edges
;  (length (run* (s id)
;            (edge id s "UMLS:C0005767")
;            (eprop id "edge_label" "affects")))
;  2414)
