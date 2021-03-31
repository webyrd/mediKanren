#lang racket
(provide (all-defined-out))
(require "../../../../pieces-parts/query.rkt"
         "../../open-api/api-query.rkt"
         ;"../molepro-api-query.rkt"
         racket/engine)

(define crosskg-mondo-disease-curies
  '("MONDO:0016192"
    "DOID:0050890"
    "MONDO:0016153"
    "MONDO:0016147"    
    "MONDO:0016191"
    "MONDO:0016196"
    "MONDO:0016187"))

(define CHEBI--negatively-regulates-->PR-->MONDO
  (curies/query
   (time (query/graph
          ((Drug #f)
           (PR #f)
           ;(Disease "MONDO:0000510")
           (Disease "MONDO:0016147")
           )
          ((Drug->PR (list "biolink:negatively_regulates_entity_to_entity") (edge/db? 'textminingprovider)) 
           (PR->Disease
            (list
             "gene_mapped_to_disease"
             "gene_involved_in_pathogenesis_of_disease"
             "gene_associated_with_disease"
             "associated_with_disease"
             "INVERTED:disease_has_basis_in_dysfunction_of") (edge/db? 'rtx2_2020_09_16)))
          (Drug Drug->PR PR)                         
          (PR PR->Disease Disease)))
   'Drug))

(define CHEBI--postively-regulates-->PR-->MONDO
  (curies/query
   (time (query/graph
          ((Drug #f)
           (PR #f)
           (Disease "MONDO:0000510"))
          ((Drug->PR (list "biolink:positively_regulates_entity_to_entity") (edge/db? 'textminingprovider)) 
           (PR->Disease
            (list
             "gene_mapped_to_disease"
             "gene_involved_in_pathogenesis_of_disease"
             "gene_associated_with_disease"
             "associated_with_disease"
             "INVERTED:disease_has_basis_in_dysfunction_of") (edge/db? 'rtx2_2020_09_16)))
          (Drug Drug->PR PR)               
          (PR PR->Disease Disease)))
   'Drug))

#|
(filter (lambda (x) (string-prefix? x "CHEMBL:"))
        (flatten (map set->list (map curie-synonyms CHEBI--negatively-regulates-->PR-->MONDO))))


|#




#;(pretty-print
 CHEBI--postively-regulates-->PR-->MONDO)

#;(pretty-print
 (time
  (api-query
   (string-append (string-append url.broad_chembl chembl-drug-indications/transform))
   (js-query/transform
    'chembl
    "ChEMBL:CHEMBL1433"))))

#|
(define json->curie-ls
  (lambda (json-str curie-prefix)
    (map (lambda (ls) (string))
         (map jsexpr->string json-str))))

(define js
  '{"size": 2731, "severity": {"operator": "=", "value": 0}, "drugs": [{"drug": "Prednisone", "identifier": "CHEMBL:CHEMBL635", "frequency": 282}, {"drug": "Fluticasone", "identifier": "CHEMBL:CHEMBL1473", "frequency": 79}, {"drug": "Mometasone", "identifier": "CHEMBL:CHEMBL1161", "frequency": 7}, {"drug": "Budesonide", "identifier": "CHEMBL:CHEMBL1370", "frequency": 36}, {"drug": "Beclomethasone", "identifier": "CHEMBL:CHEMBL1586", "frequency": 15}, {"drug": "Ciclesonide", "identifier": "CHEMBL:CHEMBL2040682", "frequency": 2}, {"drug": "Flunisolide", "identifier": "CHEMBL:CHEMBL1512", "frequency": 0}, {"drug": "Albuterol", "identifier": "CHEMBL:CHEMBL1002", "frequency": 515}, {"drug": "Metaproterenol", "identifier": "PUBCHEM:4086", "frequency": 0}, {"drug": "Diphenhydramine", "identifier": "CHEMBL:CHEMBL657", "frequency": 58}, {"drug": "Fexofenadine", "identifier": "CHEMBL:CHEMBL914", "frequency": 34}, {"drug": "Cetirizine", "identifier":
"CHEMBL:CHEMBL896", "frequency": 69}, {"drug": "Ipratropium", "identifier": "CHEMBL:CHEMBL1464005", "frequency": 84}, {"drug": "Salmeterol", "identifier": "CHEMBL:CHEMBL1263", "frequency": 167}, {"drug": "Arformoterol", "identifier": "CHEMBL:CHEMBL1363", "frequency": 2}, {"drug": "Formoterol", "identifier": "CHEMBL:CHEMBL605993", "frequency": 32}, {"drug": "Indacaterol", "identifier": "CHEMBL:CHEMBL1346", "frequency": 0}, {"drug": "Theophylline", "identifier": "CHEMBL:CHEMBL589251", "frequency": 2}, {"drug": "Omalizumab", "identifier": "CHEMBL:CHEMBL1201589", "frequency": 0}, {"drug": "Mepolizumab", "identifier": "CHEMBL:CHEMBL2108429", "frequency": 0}, {"drug": "Sertraline", "identifier": "RXCUI:155137", "frequency": 0}, {"drug": "Citalopram", "identifier": "RXCUI:221078", "frequency": 0}, {"drug": "Fluoxetine", "identifier": "RXCUI:227224", "frequency": 0}, {"drug": "Paroxetine", "identifier": "RXCUI:32937", "frequency": 0}, {"drug": "Trazodone", "identifier": "RXCUI:82112", "frequency": 0}, {"drug": "Escitalopram", "identifier": "RXCUI:353108", "frequency": 0}, {"drug": "Duloxetine", "identifier": "RXCUI:476250", "frequency": 0}, {"drug": "Venlafaxine", "identifier": "RXCUI:39786", "frequency": 0}, {"drug": "Propranolol", "identifier": "RXCUI:8787", "frequency": 0}, {"drug": "Hydroxyzine", "identifier": "RXCUI:5553", "frequency": 0}, {"drug": "Estradiol", "identifier": "RXCUI:4083", "frequency": 0}, {"drug": "Estropipate", "identifier": "RXCUI:33747", "frequency": 0}, {"drug": "Estrogen", "identifier": "RXCUI:4100", "frequency": 0}, {"drug": "Progesterone", "identifier": "RXCUI:8727", "frequency": 0}, {"drug": "Medroxyprogresterone", "identifier": "RXCUI:6691", "frequency": 0}, {"drug": "Testosterone", "identifier": "RXCUI:10379", "frequency": 0}, {"drug": "Androstenedione", "identifier": "RXCUI:784", "frequency": 0}, {"drug": "Nandrolone", "identifier": "RXCUI:7244", "frequency": 0}, {"drug": "Prasterone", "identifier": "RXCUI:3143", "frequency": 0}, {"drug": "Leuprolide", "identifier": "RXCUI:42375", "frequency": 0}, {"drug": "Goserelin", "identifier": "RXCUI:50610", "frequency": 0}, {"drug": "Triptorelin", "identifier": "RXCUI:38782", "frequency": 0}, {"drug": "Histrelin", "identifier": "RXCUI:50975", "frequency": 0}, {"drug": "Tamoxifen", "identifier": "RXCUI:10324", "frequency": 0}]})

|#
