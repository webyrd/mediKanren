With Chris Mungall (BioHackathon 2019, Fukuoka, Japan)

YAML:
https://github.com/biolink/biolink-model/blob/master/biolink-model.yaml

Raw YAML:
https://raw.githubusercontent.com/biolink/biolink-model/master/biolink-model.yaml

Predicates can be found in YAML file


(run* (type)
  (type-infero '(imatinib treats aspirin) type))
=>
()
or
Association

(run* (type)
  (type-infero '(imatinib treats cancer) type))
=>
ChemicalToDiseaseOrPhenotypicFeatureAssociation

(run* (disease)
  (type-infero `(imatinib treats ,disease)
               'ChemicalToDiseaseOrPhenotypicFeatureAssociation))

(run* (X type)
  (type-infero `(conj (imatinib treats ,X) (,X causes asthma))
               type))
=>
this is okay

(run* (X Y type)
  (type-infero `(conj (imatinib treats ,X) (,X orthologous-to ,Y))
               type))
=>
()


Pay attention to domain and range:


  increases expression of:
    description: >-
      holds between two molecular entities where the action or effect of one increases the level of expression of the other within a system of interest
    is_a: affects expression of
    in_subset:
      - translator_minimal
    domain: molecular entity
    range: genomic entity
