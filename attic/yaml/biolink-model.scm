(define biolink-model
'(
  (
    "id"
    . "https://w3id.org/biolink/biolink-model"
  )
  (
    "name"
    . "biolink_model"
  )
  (
    "description"
    . "Entity and association taxonomy and datamodel for life-sciences data"
  )
  (
    "license"
    . "https://creativecommons.org/publicdomain/zero/1.0/"
  )
  (
    "prefixes"
      (
        "biolink"
        . "https://w3id.org/biolink/vocab/"
      )
      (
        "biolinkml"
        . "https://w3id.org/biolink/biolinkml/"
      )
      (
        "OBAN"
        . "http://purl.org/oban/"
      )
      (
        "SIO"
        . "http://semanticscience.org/resource/SIO_"
      )
      (
        "wgs"
        . "http://www.w3.org/2003/01/geo/wgs84_pos"
      )
      (
        "UMLSSG"
        . "https://uts-ws.nlm.nih.gov/rest/semantic-network/semantic-network/current/GROUP/"
      )
      (
        "UMLSST"
        . "https://uts-ws.nlm.nih.gov/rest/semantic-network/semantic-network/current/STY/"
      )
      (
        "UMLSSC"
        . "https://uts-ws.nlm.nih.gov/rest/semantic-network/semantic-network/current/TUI/"
      )
  )
  (
    "default_prefix"
    . "biolink"
  )
  (
    "default_range"
    . "string"
  )
  (
    "default_curi_maps"
      "obo_context"
      "monarch_context"
      "idot_context"
      "semweb_context"
  )
  (
    "emit_prefixes"
      "rdf"
      "rdfs"
      "xsd"
      "skos"
      "OIO"
      "BIOGRID"
  )
  (
    "subsets"
      (
        "translator_minimal"
          (
            "description"
            . "Minimum subset of translator work"
          )
      )
      (
        "testing"
          (
            "description"
            . "(tbd)"
          )
      )
  )
  (
    "imports"
      "biolinkml:types"
  )
  (
    "types"
      (
        "chemical formula value"
          (
            "uri"
            . "xsd:string"
          )
          (
            "base"
            . "str"
          )
          (
            "description"
            . "A chemical formula"
          )
          (
            "notes"
              "Should be implemented as a stronger type"
          )
      )
      (
        "identifier type"
          (
            "base"
            . "ElementIdentifier"
          )
          (
            "uri"
            . "xsd:anyURI"
          )
          (
            "description"
            . "A string that is intended to uniquely identify a thing May be URI in full or compact (CURIE) form"
          )
      )
      (
        "iri type"
          (
            "typeof"
            . "uriorcurie"
          )
          (
            "description"
            . "An IRI"
          )
      )
      (
        "label type"
          (
            "typeof"
            . "string"
          )
          (
            "description"
            . "A string that provides a human-readable name for a thing"
          )
      )
      (
        "narrative text"
          (
            "typeof"
            . "string"
          )
          (
            "description"
            . "A string that provides a human-readable description of something"
          )
      )
      (
        "symbol type"
          (
            "typeof"
            . "string"
          )
      )
      (
        "frequency"
          (
            "typeof"
            . "string"
          )
          (
            "uri"
            . "UO:0000105"
          )
      )
      (
        "perecentage frequency value"
          (
            "typeof"
            . "double"
          )
          (
            "uri"
            . "UO:0000187"
          )
      )
      (
        "quotient"
          (
            "aliases"
              "ratio"
          )
          (
            "typeof"
            . "double"
          )
          (
            "uri"
            . "UO:0010006"
          )
      )
      (
        "unit"
          (
            "typeof"
            . "string"
          )
          (
            "uri"
            . "UO:0000000"
          )
          (
            "id_prefixes"
              "UO"
          )
      )
      (
        "time type"
          (
            "typeof"
            . "time"
          )
      )
      (
        "biological sequence"
          (
            "typeof"
            . "string"
          )
      )
  )
  (
    "slots"
      (
        "related to"
          (
            "description"
            . "A relationship that is asserted between two named things"
          )
          (
            "domain"
            . "named thing"
          )
          (
            "range"
            . "named thing"
          )
          (
            "multivalued"
            . #t
          )
          (
            "slot_uri"
            . "owl:ObjectProperty"
          )
          (
            "inherited"
            . #t
          )
          (
            "mappings"
              "SEMMEDDB:ASSOCIATED_WITH"
          )
      )
      (
        "interacts with"
          (
            "domain"
            . "named thing"
          )
          (
            "range"
            . "named thing"
          )
          (
            "description"
            . "holds between any two entities that directly or indirectly interact with each other"
          )
          (
            "is_a"
            . "related to"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002434"
          )
      )
      (
        "physically interacts with"
          (
            "is_a"
            . "interacts with"
          )
          (
            "description"
            . "holds between two entities that make physical contact as part of some interaction"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "symmetric"
            . #t
          )
          (
            "slot_uri"
            . "WD:P129"
          )
          (
            "mappings"
              "SEMMEDDB:INTERACTS_WITH"
          )
      )
      (
        "molecularly interacts with"
          (
            "is_a"
            . "physically interacts with"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002436"
          )
      )
      (
        "genetically interacts with"
          (
            "is_a"
            . "interacts with"
          )
          (
            "description"
            . "holds between two genes whose phenotypic effects are dependent on each other in some way - such that their combined phenotypic effects are the result of some interaction between the activity of their gene products. Examples include epistasis and synthetic lethality."
          )
          (
            "domain"
            . "gene"
          )
          (
            "range"
            . "gene"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002435"
          )
      )
      (
        "affects"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "describes an entity that has a direct affect on the state or quality of another existing entity. Use of the 'affects' predicate implies that the affected entity already exists, unlike predicates such as 'affects risk for' and 'prevents, where the outcome is something that may or may not come to be."
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "SEMMEDDB:AFFECTS"
          )
      )
      (
        "affects abundance of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one changes the amount of the other within a system of interest"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases abundance of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the amount of the other within a system of interest"
          )
          (
            "is_a"
            . "affects abundance of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases abundance of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the amount of the other within a system of interest"
          )
          (
            "is_a"
            . "affects abundance of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "affects activity of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one changes the activity of the other within a system of interest"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases activity of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the activity of the other within a system of interest"
          )
          (
            "is_a"
            . "affects activity of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases activity of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the activity of the other within a system of interest"
          )
          (
            "is_a"
            . "affects activity of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "affects expression of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one changes the level of expression of the other within a system of interest"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "genomic entity"
          )
      )
      (
        "increases expression of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the level of expression of the other within a system of interest"
          )
          (
            "is_a"
            . "affects expression of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "genomic entity"
          )
      )
      (
        "decreases expression of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the level of expression of the other within a system of interest"
          )
          (
            "is_a"
            . "affects expression of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "genomic entity"
          )
      )
      (
        "affects folding of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one changes the rate or quality of folding of the other"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases folding of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the rate or quality of folding of the other"
          )
          (
            "is_a"
            . "affects folding of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases folding of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the rate or quality of folding of the other"
          )
          (
            "is_a"
            . "affects folding of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "affects localization of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one changes the localization of the other within a system of interest"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases localization of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the proper localization of the other within a system of interest"
          )
          (
            "is_a"
            . "affects localization of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases localization of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the proper localization of the other within a system of interest"
          )
          (
            "is_a"
            . "affects localization of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "affects metabolic processing of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one impacts the metabolic processing of the other within a system of interest"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases metabolic processing of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the rate of metabolic processing of the other within a system of interest"
          )
          (
            "is_a"
            . "affects metabolic processing of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases metabolic processing of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the rate of metabolic processing of the other within a system of interest"
          )
          (
            "is_a"
            . "affects metabolic processing of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "affects molecular modification of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one leads changes in the molecular modification(s) of the other (e.g. via post-translational modifications of proteins such as the addition of phosphoryl group, or via redox reaction that adds or subtracts electrons)"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases molecular modification of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one leads to increased molecular modification(s) of the other (e.g. via post-translational modifications of proteins such as the addition of phosphoryl group, or via redox reaction that adds or subtracts electrons)"
          )
          (
            "is_a"
            . "affects molecular modification of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases molecular modification of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one leads to decreased molecular modification(s) of the other (e.g. via post-translational modifications of proteins such as the addition of phosphoryl group, or via redox reaction that adds or subtracts electrons)"
          )
          (
            "is_a"
            . "affects molecular modification of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "affects synthesis of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one impacts the rate of chemical synthesis of the other"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases synthesis of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the rate of chemical synthesis of the other"
          )
          (
            "is_a"
            . "affects synthesis of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases synthesis of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the rate of chemical synthesis of the other"
          )
          (
            "is_a"
            . "affects synthesis of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "affects degradation of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one impacts the rate of degradation of the other within a system of interest"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases degradation of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the rate of degradation of the other within a system of interest"
          )
          (
            "is_a"
            . "affects degradation of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases degradation of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the rate of degradation of the other within a system of interest"
          )
          (
            "is_a"
            . "affects degradation of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "affects mutation rate of"
          (
            "description"
            . "holds between a molecular entity and a genomic entity where the action or effect of the molecular entity impacts the rate of mutation of the genomic entity within a system of interest"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "genomic entity"
          )
      )
      (
        "increases mutation rate of"
          (
            "description"
            . "holds between a molecular entity and a genomic entity where the action or effect of the molecular entity increases the rate of mutation of the genomic entity within a system of interest"
          )
          (
            "is_a"
            . "affects mutation rate of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "genomic entity"
          )
      )
      (
        "decreases mutation rate of"
          (
            "description"
            . "holds between a molecular entity and a genomic entity where the action or effect of the molecular entity decreases the rate of mutation of the genomic entity within a system of interest"
          )
          (
            "is_a"
            . "affects mutation rate of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "genomic entity"
          )
      )
      (
        "affects response to"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one impacts the susceptibility of a biological entity or system (e.g. an organism, cell, cellular component, macromolecular machine, biological or pathological process) to the other"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases response to"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the susceptibility of a biological entity or system (e.g. an organism, cell, cellular component, macromolecular machine, biological or pathological process) to the other"
          )
          (
            "is_a"
            . "affects response to"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases response to"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the susceptibility of a biological entity or system (e.g. an organism, cell, cellular component, macromolecular machine, biological or pathological process) to the other"
          )
          (
            "is_a"
            . "affects response to"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "affects splicing of"
          (
            "description"
            . "holds between a molecular entity and an mRNA where the action or effect of the molecular entity impacts the splicing of the mRNA"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "transcript"
          )
      )
      (
        "increases splicing of"
          (
            "description"
            . "holds between a molecular entity and an mRNA where the action or effect of the molecular entity increases the proper splicing of the mRNA"
          )
          (
            "is_a"
            . "affects splicing of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "transcript"
          )
      )
      (
        "decreases splicing of"
          (
            "description"
            . "holds between a molecular entity and an mRNA where the action or effect of the molecular entity decreases the proper splicing of the mRNA"
          )
          (
            "is_a"
            . "affects splicing of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "transcript"
          )
      )
      (
        "affects stability of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one impacts the stability of the other within a system of interest"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases stability of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the stability of the other within a system of interest"
          )
          (
            "is_a"
            . "affects stability of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases stability of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the stability of the other within a system of interest"
          )
          (
            "is_a"
            . "affects stability of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "affects transport of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one impacts the rate of transport of the other across some boundary in a system of interest"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases transport of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the rate of transport of the other across some boundary in a system of interest"
          )
          (
            "is_a"
            . "affects transport of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases transport of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the rate of transport of the other across some boundary in a system of interest"
          )
          (
            "is_a"
            . "affects transport of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "affects secretion of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one impacts the rate of secretion of the other out of a cell, gland, or organ"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases secretion of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the rate of secretion of the other out of a cell, gland, or organ"
          )
          (
            "is_a"
            . "affects secretion of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases secretion of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the rate of secretion of the other out of a cell, gland, or organ"
          )
          (
            "is_a"
            . "affects secretion of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "affects uptake of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one impacts the rate of uptake of the other into of a cell, gland, or organ"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "increases uptake of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one increases the rate of uptake of the other into of a cell, gland, or organ"
          )
          (
            "is_a"
            . "affects uptake of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "decreases uptake of"
          (
            "description"
            . "holds between two molecular entities where the action or effect of one decreases the rate of uptake of the other into of a cell, gland, or organ"
          )
          (
            "is_a"
            . "affects uptake of"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
      )
      (
        "regulates"
          (
            "is_a"
            . "affects"
          )
          (
            "comments"
              "This is a grouping for process-process and entity-entity relations"
          )
          (
            "mixin"
            . #t
          )
          (
            "abstract"
            . #t
          )
          (
            "slot_uri"
            . "WD:P128"
          )
      )
      (
        "positively regulates"
          (
            "comments"
              "This is a grouping for process-process and entity-entity relations"
          )
          (
            "is_a"
            . "regulates"
          )
          (
            "mixin"
            . #t
          )
          (
            "abstract"
            . #t
          )
      )
      (
        "negatively regulates"
          (
            "comments"
              "This is a grouping for process-process and entity-entity relations"
          )
          (
            "is_a"
            . "regulates"
          )
          (
            "mixin"
            . #t
          )
          (
            "abstract"
            . #t
          )
      )
      (
        "regulates, process to process"
          (
            "is_a"
            . "regulates"
          )
          (
            "domain"
            . "occurrent"
          )
          (
            "range"
            . "occurrent"
          )
          (
            "slot_uri"
            . "RO:0002211"
          )
      )
      (
        "positively regulates, process to process"
          (
            "is_a"
            . "regulates, process to process"
          )
          (
            "mixins"
              "positively regulates"
          )
          (
            "slot_uri"
            . "RO:0002213"
          )
      )
      (
        "negatively regulates, process to process"
          (
            "is_a"
            . "regulates, process to process"
          )
          (
            "mixins"
              "negatively regulates"
          )
          (
            "slot_uri"
            . "RO:0002212"
          )
      )
      (
        "regulates, entity to entity"
          (
            "aliases"
              "activity directly regulates activity of"
          )
          (
            "is_a"
            . "regulates"
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "molecular entity"
          )
          (
            "local_names"
              (
                "translator"
                . "regulates"
              )
              (
                "ro"
                . "activity directly regulates activity of"
              )
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002448"
          )
      )
      (
        "positively regulates, entity to entity"
          (
            "aliases"
              "activity directly positively regulates activity of"
          )
          (
            "is_a"
            . "regulates, entity to entity"
          )
          (
            "mixins"
              "positively regulates"
          )
          (
            "local_names"
              (
                "translator"
                . "positively regulates"
              )
              (
                "ro"
                . "activity directly positively regulates activity of"
              )
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002450"
          )
          (
            "mappings"
              "SEMMEDDB:STIMULATES"
          )
      )
      (
        "negatively regulates, entity to entity"
          (
            "aliases"
              "activity directly negatively regulates activity of"
          )
          (
            "is_a"
            . "regulates, entity to entity"
          )
          (
            "mixins"
              "negatively regulates"
          )
          (
            "local_names"
              (
                "translator"
                . "negatively regulates"
              )
              (
                "ro"
                . "activity directly negatively regulates activity of"
              )
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002449"
          )
          (
            "mappings"
              "SEMMEDDB:INHIBITS"
          )
      )
      (
        "disrupts"
          (
            "is_a"
            . "affects"
          )
          (
            "description"
            . "describes a relationship where one entity degrades or interferes with the structure, function, or occurrence of another."
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "SEMMEDDB:DISRUPTS"
          )
      )
      (
        "has gene product"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between a gene and a transcribed and/or translated product generated from it"
          )
          (
            "domain"
            . "gene"
          )
          (
            "range"
            . "gene product"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002205"
          )
          (
            "mappings"
              "WD:P688"
          )
      )
      (
        "homologous to"
          (
            "is_a"
            . "related to"
          )
          (
            "aliases"
              "in homology relationship with"
          )
          (
            "description"
            . "holds between two biological entities that have common evolutionary origin"
          )
          (
            "comments"
              "typically used to describe homology relationships between genes or gene products"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:HOM0000001"
          )
          (
            "mappings"
              "SIO:010302"
          )
      )
      (
        "paralogous to"
          (
            "is_a"
            . "homologous to"
          )
          (
            "description"
            . "a homology relationship that holds between entities (typically genes) that diverged after a duplication event."
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:HOM0000011"
          )
      )
      (
        "orthologous to"
          (
            "is_a"
            . "homologous to"
          )
          (
            "description"
            . "a homology relationship between entities (typically genes) that diverged after a speciation event."
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:HOM0000017"
          )
          (
            "mappings"
              "WD:P684"
          )
      )
      (
        "xenologous to"
          (
            "is_a"
            . "homologous to"
          )
          (
            "description"
            . "a homology relationship characterized by an interspecies (horizontal) transfer since the common ancestor."
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:HOM0000018"
          )
      )
      (
        "coexists with"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between two entities that are co-located in the same aggregate object, process, or spatio-temporal region"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "SEMMEDDB:COEXISTS_WITH"
          )
      )
      (
        "in pathway with"
          (
            "description"
            . "holds between two genes or gene products that are part of in the same biological pathway"
          )
          (
            "is_a"
            . "coexists with"
          )
          (
            "domain"
            . "gene or gene product"
          )
          (
            "range"
            . "gene or gene product"
          )
          (
            "in_subset"
              "translator_minimal"
          )
      )
      (
        "in complex with"
          (
            "description"
            . "holds between two genes or gene products that are part of (or code for products that are part of) in the same macromolecular complex"
          )
          (
            "is_a"
            . "coexists with"
          )
          (
            "domain"
            . "gene or gene product"
          )
          (
            "range"
            . "gene or gene product"
          )
          (
            "in_subset"
              "translator_minimal"
          )
      )
      (
        "in cell population with"
          (
            "description"
            . "holds between two genes or gene products that are expressed in the same cell type or population"
          )
          (
            "is_a"
            . "coexists with"
          )
          (
            "domain"
            . "gene or gene product"
          )
          (
            "range"
            . "gene or gene product"
          )
          (
            "in_subset"
              "translator_minimal"
          )
      )
      (
        "colocalizes with"
          (
            "description"
            . "holds between two entities that are observed to be located in the same place."
          )
          (
            "is_a"
            . "coexists with"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:00002325"
          )
      )
      (
        "gene associated with condition"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between a gene and a disease or phenotypic feature that the gene or its alleles/products may influence, contribute to, or correlate with"
          )
          (
            "domain"
            . "gene"
          )
          (
            "range"
            . "disease or phenotypic feature"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "WD:P2293"
          )
      )
      (
        "affects risk for"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between two entities where exposure to one entity alters the chance of developing the other"
          )
          (
            "in_subset"
              "translator_minimal"
          )
      )
      (
        "predisposes"
          (
            "is_a"
            . "affects risk for"
          )
          (
            "description"
            . "holds between two entities where exposure to one entity increases the chance of developing the other"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "SEMMEDDB:PREDISPOSES"
          )
      )
      (
        "contributes to"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between two entities where the occurrence, existence, or activity of one causes or contributes to the occurrence or generation of the other"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002326"
          )
      )
      (
        "causes"
          (
            "description"
            . "holds between two entities where the occurrence, existence, or activity of one causes the occurrence or  generation of the other"
          )
          (
            "is_a"
            . "contributes to"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002410"
          )
          (
            "mappings"
              "SEMMEDDB:CAUSES"
              "WD:P1542"
          )
      )
      (
        "treats"
          (
            "aliases"
              "is substance that treats"
          )
          (
            "is_a"
            . "affects"
          )
          (
            "description"
            . "holds between a therapeutic procedure or chemical substance and a disease or phenotypic feature that it is used to treat"
          )
          (
            "domain"
            . "treatment"
          )
          (
            "required"
            . #t
          )
          (
            "range"
            . "disease or phenotypic feature"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "treated by"
          )
          (
            "slot_uri"
            . "RO:0002606"
          )
          (
            "mappings"
              "RO:0003307"
              "SEMMEDDB:TREATS"
              "WD:P2175"
          )
      )
      (
        "prevents"
          (
            "is_a"
            . "affects risk for"
          )
          (
            "description"
            . "holds between an entity whose application or use reduces the likelihood of a potential outcome. Typically used to associate a chemical substance, exposure, activity, or medical intervention that can prevent the onset a disease or phenotypic feature."
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002599"
          )
          (
            "mappings"
              "SEMMEDDB:PREVENTS"
          )
      )
      (
        "correlated with"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between a disease or phenotypic feature and a measurable molecular entity that is used as an indicator of the presence or state of the disease or feature."
          )
          (
            "domain"
            . "disease or phenotypic feature"
          )
          (
            "range"
            . "molecular entity"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002610"
          )
      )
      (
        "has biomarker"
          (
            "is_a"
            . "correlated with"
          )
          (
            "description"
            . "holds between a disease or phenotypic feature and a measurable molecular entity that is used as an indicator of the presence or state of the disease or feature."
          )
          (
            "domain"
            . "disease or phenotypic feature"
          )
          (
            "range"
            . "molecular entity"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "biomarker for"
          )
      )
      (
        "biomarker for"
          (
            "is_a"
            . "correlated with"
          )
          (
            "description"
            . "holds between a measurable molecular entity and a disease or phenotypic feature, where the entity is used as an indicator of the presence or state of the disease or feature."
          )
          (
            "domain"
            . "molecular entity"
          )
          (
            "range"
            . "disease or phenotypic feature"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "has biomarker"
          )
          (
            "slot_uri"
            . "RO:0002607"
          )
      )
      (
        "treated by"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between a disease or phenotypic feature and a therapeutic process or chemical substance that is used to treat the condition"
          )
          (
            "domain"
            . "disease or phenotypic feature"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "treats"
          )
          (
            "slot_uri"
            . "RO:0002302"
          )
          (
            "mappings"
              "WD:P2176"
          )
      )
      (
        "expressed in"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between a gene or gene product and an anatomical entity in which it is expressed"
          )
          (
            "domain"
            . "gene or gene product"
          )
          (
            "range"
            . "anatomical entity"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "expresses"
          )
          (
            "slot_uri"
            . "RO:0002206"
          )
      )
      (
        "expresses"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between an anatomical entity and gene or gene product that is expressed there"
          )
          (
            "domain"
            . "anatomical entity"
          )
          (
            "range"
            . "gene or gene product"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "expressed in"
          )
          (
            "slot_uri"
            . "RO:0002292"
          )
      )
      (
        "has phenotype"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between a biological entity and a phenotype, where a phenotype is construed broadly as any kind of quality of an organism part, a collection of these qualities, or a change in quality or qualities (e.g. abnormally increased temperature)."
          )
          (
            "domain"
            . "biological entity"
          )
          (
            "range"
            . "phenotypic feature"
          )
          (
            "notes"
              "check the range"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002200"
          )
      )
      (
        "occurs in"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between a process and a material entity or site within which the process occurs"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "BFO:0000066"
          )
          (
            "mappings"
              "SEMMEDDB:OCCURS_IN"
              "SEMMEDDB:PROCESS_OF"
          )
      )
      (
        "located in"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between a material entity and a material entity or site within which it is located (but of which it is not considered a part)"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "location of"
          )
          (
            "slot_uri"
            . "RO:0001025"
          )
      )
      (
        "location of"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between material entity or site and a material entity that is located within it (but not considered a part of it)"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "located in"
          )
          (
            "slot_uri"
            . "RO:0001015"
          )
          (
            "mappings"
              "SEMMEDDB:LOCATION_OF"
              "WD:276"
          )
      )
      (
        "model of"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between an entity and some other entity it approximates for purposes of scientific study, in virtue of its exhibiting similar features of the studied entity."
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0003301"
          )
      )
      (
        "overlaps"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between entties that overlap in their extents (materials or processes)"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002131"
          )
      )
      (
        "has part"
          (
            "is_a"
            . "overlaps"
          )
          (
            "description"
            . "holds between wholes and their parts (material entities or processes)"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "part of"
          )
          (
            "slot_uri"
            . "BFO:0000051"
          )
          (
            "mappings"
              "WD:P527"
          )
      )
      (
        "part of"
          (
            "is_a"
            . "overlaps"
          )
          (
            "description"
            . "holds between parts and wholes (material entities or processes)"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "has part"
          )
          (
            "slot_uri"
            . "BFO:0000050"
          )
          (
            "mappings"
              "SEMMEDDB:PART_OF"
              "WD:P361"
          )
      )
      (
        "has participant"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between a process and a continuant, where the continuant is somehow involved in the process"
          )
          (
            "domain"
            . "occurrent"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "participates in"
          )
          (
            "slot_uri"
            . "RO:0000057"
          )
          (
            "mappings"
              "WD:P2283"
          )
      )
      (
        "has input"
          (
            "is_a"
            . "has participant"
          )
          (
            "description"
            . "holds between a process and a continuant, where the continuant is an input into the process"
          )
          (
            "domain"
            . "occurrent"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002233"
          )
          (
            "mappings"
              "SEMMEDDB:USES"
          )
      )
      (
        "participates in"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between a continuant and a process, where the continuant is somehow involved in the process"
          )
          (
            "range"
            . "occurrent"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "has participant"
          )
          (
            "slot_uri"
            . "RO:0000056"
          )
      )
      (
        "actively involved in"
          (
            "is_a"
            . "participates in"
          )
          (
            "description"
            . "holds between a continuant and a process or function, where the continuant actively contributes to part or all of the process or function it realizes"
          )
          (
            "range"
            . "occurrent"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002331"
          )
      )
      (
        "capable of"
          (
            "is_a"
            . "actively involved in"
          )
          (
            "description"
            . "holds between a continuant and process or function, where the continuant alone has the ability to carry out the process or function."
          )
          (
            "range"
            . "occurrent"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002215"
          )
      )
      (
        "derives into"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between two distinct material entities, the old entity and the new entity, in which the new entity begins to exist when the old entity ceases to exist, and the new entity inherits the significant portion of the matter of the old entity"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "derives from"
          )
          (
            "slot_uri"
            . "RO:0001001"
          )
          (
            "mappings"
              "SEMMEDDB:CONVERTS_TO"
          )
      )
      (
        "derives from"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between two distinct material entities, the new entity and the old entity, in which the new entity begins to exist when the old entity ceases to exist, and the new entity inherits the significant portion of the matter of the old entity"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "inverse"
            . "derives into"
          )
          (
            "slot_uri"
            . "RO:0001000"
          )
      )
      (
        "manifestation of"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "used in SemMedDB for linking things like dysfunctions and processes to some disease or syndrome"
          )
          (
            "range"
            . "disease"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "SEMMEDDB:MANIFESTATION_OF"
          )
          (
            "mappings"
              "WD:P1557"
          )
      )
      (
        "produces"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between a material entity and a product that is generated through the intentional actions or functioning of the material entity"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0003000"
          )
          (
            "mappings"
              "WD:P1056"
              "SEMMEDDB:PRODUCES"
          )
      )
      (
        "precedes"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between two processes, where one completes before the other begins"
          )
          (
            "domain"
            . "occurrent"
          )
          (
            "range"
            . "occurrent"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "BFO:0000063"
          )
          (
            "mappings"
              "SEMMEDDB:PRECEDES"
              "WD:P156"
          )
      )
      (
        "same as"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between two entities that are considered equivalent to each other"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "owl:equivalentClass"
          )
          (
            "mappings"
              "owl:sameAs"
              "skos:exactMatch"
              "WD:P2888"
          )
      )
      (
        "subclass of"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "holds between two classes where the domain class is a specialization of the range class"
          )
          (
            "domain"
            . "ontology class"
          )
          (
            "range"
            . "iri type"
          )
          (
            "multivalued"
            . #t
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "rdfs:subClassOf"
          )
          (
            "mappings"
              "SEMMEDDB:IS_A"
              "WD:P279"
          )
      )
      (
        "node property"
          (
            "description"
            . "A grouping for any property that holds between a node and a value"
          )
          (
            "domain"
            . "named thing"
          )
      )
      (
        "title"
          (
            "is_a"
            . "node property"
          )
          (
            "domain"
            . "data set version"
          )
          (
            "slot_uri"
            . "dct:title"
          )
      )
      (
        "source data file"
          (
            "is_a"
            . "node property"
          )
          (
            "domain"
            . "data set version"
          )
          (
            "range"
            . "data file"
          )
          (
            "slot_uri"
            . "dcterms:source"
          )
      )
      (
        "source web page"
          (
            "is_a"
            . "node property"
          )
          (
            "domain"
            . "data set summary"
          )
          (
            "slot_uri"
            . "dcterms:source"
          )
      )
      (
        "retrievedOn"
          (
            "is_a"
            . "node property"
          )
          (
            "domain"
            . "source file"
          )
          (
            "range"
            . "date"
          )
          (
            "slot_uri"
            . "pav:retrievedOn"
          )
      )
      (
        "versionOf"
          (
            "is_a"
            . "node property"
          )
          (
            "domain"
            . "data set version"
          )
          (
            "range"
            . "data set"
          )
          (
            "slot_uri"
            . "dct:isVersionOf"
          )
      )
      (
        "source version"
          (
            "is_a"
            . "node property"
          )
          (
            "domain"
            . "source file"
          )
          (
            "slot_uri"
            . "pav:version"
          )
      )
      (
        "downloadURL"
          (
            "is_a"
            . "node property"
          )
          (
            "domain"
            . "distribution level"
          )
          (
            "slot_uri"
            . "dct:downloadURL"
          )
      )
      (
        "distribution"
          (
            "is_a"
            . "node property"
          )
          (
            "domain"
            . "data set version"
          )
          (
            "range"
            . "distribution level"
          )
          (
            "slot_uri"
            . "void:Dataset"
          )
      )
      (
        "type"
          (
            "is_a"
            . "node property"
          )
          (
            "slot_uri"
            . "rdf:type"
          )
      )
      (
        "id"
          (
            "is_a"
            . "node property"
          )
          (
            "identifier"
            . #t
          )
          (
            "description"
            . "A unique identifier for a thing. Must be either a CURIE shorthand for a URI or a complete URI"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "named thing"
          )
          (
            "range"
            . "identifier type"
          )
          (
            "required"
            . #t
          )
      )
      (
        "association_id"
          (
            "is_a"
            . "node property"
          )
          (
            "identifier"
            . #t
          )
          (
            "description"
            . "A unique identifier for an association"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "association"
          )
          (
            "range"
            . "identifier type"
          )
          (
            "ifabsent"
            . "bnode"
          )
          (
            "alias"
            . "id"
          )
      )
      (
        "iri"
          (
            "is_a"
            . "node property"
          )
          (
            "description"
            . "An IRI for the node. This is determined by the id using expansion rules."
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "domain"
            . "named thing"
          )
          (
            "range"
            . "iri type"
          )
      )
      (
        "name"
          (
            "is_a"
            . "node property"
          )
          (
            "aliases"
              "label"
              "display name"
          )
          (
            "domain"
            . "named thing"
          )
          (
            "range"
            . "label type"
          )
          (
            "description"
            . "A human-readable name for a thing"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "required"
            . #t
          )
          (
            "slot_uri"
            . "rdfs:label"
          )
      )
      (
        "synonym"
          (
            "is_a"
            . "node property"
          )
          (
            "aliases"
              "alias"
          )
          (
            "domain"
            . "named thing"
          )
          (
            "range"
            . "label type"
          )
          (
            "description"
            . "Alternate human-readable names for a thing"
          )
          (
            "multivalued"
            . #t
          )
          (
            "in_subset"
              "translator_minimal"
          )
      )
      (
        "category"
          (
            "is_a"
            . "node property"
          )
          (
            "domain"
            . "named thing"
          )
          (
            "range"
            . "iri type"
          )
          (
            "description"
            . "Name of the high level ontology class in which this entity is categorized. Corresponds to the label for the biolink entity type class. In a neo4j database this MAY correspond to the neo4j label tag"
          )
          (
            "is_class_field"
            . #t
          )
          (
            "multivalued"
            . #t
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "required"
            . #t
          )
          (
            "slot_uri"
            . "rdfs:subClassOf"
          )
      )
      (
        "full name"
          (
            "is_a"
            . "node property"
          )
          (
            "domain"
            . "named thing"
          )
          (
            "range"
            . "label type"
          )
          (
            "description"
            . "a long-form human readable name for a thing"
          )
      )
      (
        "description"
          (
            "is_a"
            . "node property"
          )
          (
            "domain"
            . "named thing"
          )
          (
            "range"
            . "narrative text"
          )
          (
            "description"
            . "a human-readable description of a thing"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "dcterms:description"
          )
      )
      (
        "systematic synonym"
          (
            "is_a"
            . "node property"
          )
          (
            "domain"
            . "named thing"
          )
          (
            "range"
            . "label type"
          )
          (
            "multivalued"
            . #t
          )
          (
            "description"
            . "more commonly used for gene symbols in yeast"
          )
      )
      (
        "association slot"
          (
            "abstract"
            . #t
          )
          (
            "domain"
            . "association"
          )
          (
            "aliases"
              "edge property"
              "statement property"
          )
          (
            "description"
            . "any slot that relates an association to another entity"
          )
      )
      (
        "subject"
          (
            "is_a"
            . "association slot"
          )
          (
            "local_names"
              (
                "ga4gh"
                . "annotation subject"
              )
              (
                "neo4j"
                . "node with outgoing relationship"
              )
          )
          (
            "description"
            . "connects an association to the subject of the association. For example, in a gene-to-phenotype association, the gene is subject and phenotype is object."
          )
          (
            "required"
            . #t
          )
          (
            "range"
            . "named thing"
          )
          (
            "slot_uri"
            . "rdf:subject"
          )
          (
            "mappings"
              "owl:annotatedSource"
              "oban:association_has_subject"
          )
      )
      (
        "object"
          (
            "is_a"
            . "association slot"
          )
          (
            "description"
            . "connects an association to the object of the association. For example, in a gene-to-phenotype association, the gene is subject and phenotype is object."
          )
          (
            "required"
            . #t
          )
          (
            "range"
            . "named thing"
          )
          (
            "local_names"
              (
                "ga4gh"
                . "descriptor"
              )
              (
                "neo4j"
                . "node with incoming relationship"
              )
          )
          (
            "slot_uri"
            . "rdf:object"
          )
          (
            "mappings"
              "owl:annotatedTarget"
              "oban:association_has_object"
          )
      )
      (
        "edge label"
          (
            "is_a"
            . "association slot"
          )
          (
            "description"
            . "A high-level grouping for the relationship type. AKA minimal predicate. This is analogous to category for nodes."
          )
          (
            "domain"
            . "association"
          )
          (
            "notes"
              "in neo4j this corresponds to the relationship type and the convention is for a snake_case label"
          )
          (
            "range"
            . "label type"
          )
          (
            "required"
            . #t
          )
      )
      (
        "relation"
          (
            "is_a"
            . "association slot"
          )
          (
            "description"
            . "the relationship type by which a subject is connected to an object in an association"
          )
          (
            "domain"
            . "association"
          )
          (
            "range"
            . "uriorcurie"
          )
          (
            "required"
            . #t
          )
          (
            "local_names"
              (
                "ga4gh"
                . "annotation predicate"
              )
              (
                "translator"
                . "predicate"
              )
          )
          (
            "slot_uri"
            . "rdf:predicate"
          )
          (
            "mappings"
              "owl:annotatedProperty"
              "oban:association_has_predicate"
          )
      )
      (
        "negated"
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "boolean"
          )
          (
            "description"
            . "if set to true, then the association is negated i.e. is not true"
          )
      )
      (
        "has confidence level"
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "confidence level"
          )
          (
            "description"
            . "connects an association to a qualitative term denoting the level of confidence"
          )
      )
      (
        "has evidence"
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "evidence type"
          )
          (
            "description"
            . "connects an association to an instance of supporting evidence"
          )
          (
            "slot_uri"
            . "RO:0002558"
          )
      )
      (
        "provided by"
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "provider"
          )
          (
            "description"
            . "connects an association to the agent (person, organization or group) that provided it"
          )
          (
            "slot_uri"
            . "pav:providedBy"
          )
      )
      (
        "association type"
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "ontology class"
          )
          (
            "description"
            . "connects an association to the type of association (e.g. gene to phenotype)"
          )
          (
            "slot_uri"
            . "rdf:type"
          )
      )
      (
        "creation date"
          (
            "is_a"
            . "node property"
          )
          (
            "range"
            . "date"
          )
          (
            "description"
            . "date on which thing was created. This can be applied to nodes or edges"
          )
          (
            "slot_uri"
            . "dcterms:created"
          )
      )
      (
        "update date"
          (
            "is_a"
            . "node property"
          )
          (
            "range"
            . "date"
          )
          (
            "description"
            . "date on which thing was updated. This can be applied to nodes or edges"
          )
      )
      (
        "in taxon"
          (
            "is_a"
            . "related to"
          )
          (
            "domain"
            . "thing with taxon"
          )
          (
            "range"
            . "organism taxon"
          )
          (
            "description"
            . "connects a thing to a class representing a taxon"
          )
          (
            "in_subset"
              "translator_minimal"
          )
          (
            "slot_uri"
            . "RO:0002162"
          )
          (
            "mappings"
              "WD:P703"
          )
      )
      (
        "latitude"
          (
            "is_a"
            . "node property"
          )
          (
            "range"
            . "float"
          )
          (
            "description"
            . "latitude"
          )
          (
            "slot_uri"
            . "wgs:lat"
          )
      )
      (
        "longitude"
          (
            "is_a"
            . "node property"
          )
          (
            "range"
            . "float"
          )
          (
            "description"
            . "longitude"
          )
          (
            "slot_uri"
            . "wgs:long"
          )
      )
      (
        "has chemical formula"
          (
            "is_a"
            . "node property"
          )
          (
            "range"
            . "chemical formula value"
          )
          (
            "description"
            . "description of chemical compound based on element symbols"
          )
          (
            "slot_uri"
            . "WD:P274"
          )
      )
      (
        "aggregate statistic"
          (
            "is_a"
            . "node property"
          )
          (
            "abstract"
            . #t
          )
      )
      (
        "has count"
          (
            "description"
            . "number of things with a particular property"
          )
          (
            "is_a"
            . "aggregate statistic"
          )
          (
            "range"
            . "integer"
          )
      )
      (
        "has total"
          (
            "description"
            . "total number of things in a particular reference set"
          )
          (
            "is_a"
            . "aggregate statistic"
          )
          (
            "range"
            . "integer"
          )
      )
      (
        "has quotient"
          (
            "is_a"
            . "aggregate statistic"
          )
          (
            "range"
            . "double"
          )
      )
      (
        "has percentage"
          (
            "description"
            . "equivalent to has quotient multiplied by 100"
          )
          (
            "is_a"
            . "aggregate statistic"
          )
          (
            "range"
            . "double"
          )
      )
      (
        "timepoint"
          (
            "is_a"
            . "node property"
          )
          (
            "range"
            . "time type"
          )
          (
            "description"
            . "a point in time"
          )
      )
      (
        "stage qualifier"
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "life stage"
          )
          (
            "description"
            . "stage at which expression takes place"
          )
      )
      (
        "quantifier qualifier"
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "ontology class"
          )
          (
            "description"
            . "A measurable quantity for the object of the association"
          )
      )
      (
        "qualifiers"
          (
            "singular_name"
            . "qualifier"
          )
          (
            "description"
            . "connects an association to qualifiers that modify or qualify the meaning of that association"
          )
          (
            "local_names"
              (
                "ga4gh"
                . "annotation qualifier"
              )
          )
          (
            "is_a"
            . "association slot"
          )
          (
            "multivalued"
            . #t
          )
          (
            "range"
            . "ontology class"
          )
      )
      (
        "frequency qualifier"
          (
            "description"
            . "a qualifier used in a phenotypic association to state how frequent the phenotype is observed in the subject"
          )
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "frequency value"
          )
      )
      (
        "severity qualifier"
          (
            "description"
            . "a qualifier used in a phenotypic association to state how severe the phenotype is in the subject"
          )
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "severity value"
          )
      )
      (
        "sex qualifier"
          (
            "description"
            . "a qualifier used in a phenotypic association to state whether the association is specific to a particular sex."
          )
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "biological sex"
          )
      )
      (
        "onset qualifier"
          (
            "description"
            . "a qualifier used in a phenotypic association to state when the phenotype appears is in the subject"
          )
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "onset"
          )
      )
      (
        "clinical modifier qualifier"
          (
            "description"
            . "Used to characterize and specify the phenotypic abnormalities defined in the Phenotypic abnormality subontology, with respect to severity, laterality, age of onset, and other aspects"
          )
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "clinical modifier"
          )
      )
      (
        "sequence variant qualifier"
          (
            "description"
            . "a qualifier used in an association where the variant"
          )
          (
            "is_a"
            . "association slot"
          )
          (
            "range"
            . "sequence variant"
          )
      )
      (
        "publications"
          (
            "singular_name"
            . "publication"
          )
          (
            "description"
            . "connects an association to publications supporting the association"
          )
          (
            "is_a"
            . "association slot"
          )
          (
            "multivalued"
            . #t
          )
          (
            "range"
            . "publication"
          )
      )
      (
        "has biological sequence"
          (
            "is_a"
            . "node property"
          )
          (
            "description"
            . "connects a genomic feature to its sequence"
          )
          (
            "range"
            . "biological sequence"
          )
      )
      (
        "has molecular consequence"
          (
            "is_a"
            . "related to"
          )
          (
            "description"
            . "connects a sequence variant to a class describing the molecular consequence. E.g.  SO:0001583"
          )
          (
            "range"
            . "ontology class"
          )
      )
      (
        "has gene"
          (
            "is_a"
            . "node property"
          )
          (
            "description"
            . "connects and entity to a single gene"
          )
          (
            "range"
            . "gene"
          )
      )
      (
        "has zygosity"
          (
            "is_a"
            . "node property"
          )
          (
            "range"
            . "zygosity"
          )
      )
      (
        "filler"
          (
            "is_a"
            . "node property"
          )
          (
            "range"
            . "named thing"
          )
          (
            "description"
            . "The value in a property-value tuple"
          )
      )
      (
        "phase"
          (
            "is_a"
            . "node property"
          )
          (
            "description"
            . "TODO"
          )
      )
      (
        "genome build"
          (
            "is_a"
            . "node property"
          )
          (
            "description"
            . "TODO"
          )
      )
      (
        "interbase coordinate"
          (
            "is_a"
            . "node property"
          )
          (
            "description"
            . "TODO"
          )
      )
      (
        "start interbase coordinate"
          (
            "is_a"
            . "interbase coordinate"
          )
      )
      (
        "end interbase coordinate"
          (
            "is_a"
            . "interbase coordinate"
          )
      )
  )
  (
    "classes"
      (
        "attribute"
          (
            "subclass_of"
            . "PATO:0000001"
          )
          (
            "mixins"
              "ontology class"
          )
          (
            "description"
            . "A property or characteristic of an entity"
          )
      )
      (
        "biological sex"
          (
            "subclass_of"
            . "PATO:0000047"
          )
          (
            "is_a"
            . "attribute"
          )
      )
      (
        "phenotypic sex"
          (
            "description"
            . "An attribute corresponding to the phenotypic sex of the individual, based upon the reproductive organs present."
          )
          (
            "subclass_of"
            . "PATO:0001894"
          )
          (
            "is_a"
            . "biological sex"
          )
      )
      (
        "genotypic sex"
          (
            "description"
            . "An attribute corresponding to the genotypic sex of the individual, based upon genotypic composition of sex chromosomes."
          )
          (
            "subclass_of"
            . "PATO:0020000"
          )
          (
            "is_a"
            . "biological sex"
          )
      )
      (
        "severity value"
          (
            "description"
            . "describes the severity of a phenotypic feature or disease"
          )
          (
            "is_a"
            . "attribute"
          )
      )
      (
        "frequency value"
          (
            "description"
            . "describes the frequency of occurrence of an event or condition"
          )
          (
            "is_a"
            . "attribute"
          )
      )
      (
        "clinical modifier"
          (
            "description"
            . "Used to characterize and specify the phenotypic abnormalities defined in the Phenotypic abnormality subontology, with respect to severity, laterality, age of onset, and other aspects"
          )
          (
            "subclass_of"
            . "HP:0012823"
          )
          (
            "is_a"
            . "attribute"
          )
      )
      (
        "onset"
          (
            "description"
            . "The age group in which manifestations appear"
          )
          (
            "class_uri"
            . "HP:0003674"
          )
          (
            "is_a"
            . "attribute"
          )
      )
      (
        "relationship quantifier"
          (
            "abstract"
            . #t
          )
          (
            "mixin"
            . #t
          )
      )
      (
        "sensitivity quantifier"
          (
            "is_a"
            . "relationship quantifier"
          )
          (
            "mixin"
            . #t
          )
      )
      (
        "specificity quantifier"
          (
            "is_a"
            . "relationship quantifier"
          )
          (
            "mixin"
            . #t
          )
      )
      (
        "pathognomonicity quantifier"
          (
            "is_a"
            . "specificity quantifier"
          )
          (
            "description"
            . "A relationship quantifier between a variant or symptom and a disease, which is high when the presence of the feature implies the existence of the disease"
          )
          (
            "mixin"
            . #t
          )
      )
      (
        "frequency quantifier"
          (
            "is_a"
            . "relationship quantifier"
          )
          (
            "mixin"
            . #t
          )
          (
            "slots"
              "has count"
              "has total"
              "has quotient"
              "has percentage"
          )
      )
      (
        "named thing"
          (
            "description"
            . "a databased entity or concept/class"
          )
          (
            "slots"
              "id"
              "name"
              "category"
          )
          (
            "subclass_of"
            . "BFO:0000001"
          )
          (
            "class_uri"
            . "WD:Q35120"
          )
          (
            "mappings"
              "UMLSSG:OBJC"
              "UMLSSC:T071"
              "UMLSST:enty"
              "UMLSSC:T072"
              "UMLSST:phob"
              "UMLSSC:T073"
              "UMLSST:mnob"
              "UMLSSC:T168"
              "UMLSST:food"
          )
      )
      (
        "data file"
          (
            "is_a"
            . "named thing"
          )
          (
            "class_uri"
            . "EFO:0004095"
          )
      )
      (
        "source file"
          (
            "is_a"
            . "data file"
          )
          (
            "slots"
              "source version"
              "retrievedOn"
          )
      )
      (
        "data set"
          (
            "is_a"
            . "named thing"
          )
          (
            "class_uri"
            . "IAO:0000100"
          )
      )
      (
        "data set version"
          (
            "is_a"
            . "data set"
          )
          (
            "slots"
              "title"
              "source data file"
              "versionOf"
              "type"
              "distribution"
          )
      )
      (
        "distribution level"
          (
            "is_a"
            . "data set version"
          )
          (
            "mixin"
            . #t
          )
          (
            "slots"
              "downloadURL"
          )
      )
      (
        "data set summary"
          (
            "is_a"
            . "data set version"
          )
          (
            "mixin"
            . #t
          )
          (
            "slots"
              "source web page"
          )
      )
      (
        "biological entity"
          (
            "is_a"
            . "named thing"
          )
          (
            "abstract"
            . #t
          )
          (
            "class_uri"
            . "WD:Q28845870"
          )
          (
            "mappings"
              "UMLSSC:T050"
              "UMLSST:emod"
          )
      )
      (
        "ontology class"
          (
            "is_a"
            . "named thing"
          )
          (
            "description"
            . "a concept or class in an ontology, vocabulary or thesaurus"
          )
      )
      (
        "relationship type"
          (
            "is_a"
            . "ontology class"
          )
          (
            "description"
            . "An OWL property used as an edge label"
          )
      )
      (
        "gene ontology class"
          (
            "description"
            . "an ontology class that describes a functional aspect of a gene, gene prodoct or complex"
          )
          (
            "is_a"
            . "ontology class"
          )
          (
            "in_subset"
              "testing"
          )
      )
      (
        "thing with taxon"
          (
            "abstract"
            . #t
          )
          (
            "mixin"
            . #t
          )
          (
            "description"
            . "A mixin that can be used on any entity with a taxon"
          )
          (
            "slots"
              "in taxon"
          )
      )
      (
        "organism taxon"
          (
            "is_a"
            . "ontology class"
          )
          (
            "values_from"
              "NCBITaxon"
          )
          (
            "class_uri"
            . "WD:Q16521"
          )
      )
      (
        "organismal entity"
          (
            "description"
            . "A named entity that is either a part of an organism, a whole organism, population or clade of organisms, excluding molecular entities"
          )
          (
            "abstract"
            . #t
          )
          (
            "is_a"
            . "biological entity"
          )
          (
            "class_uri"
            . "WD:Q7239"
          )
      )
      (
        "individual organism"
          (
            "mixins"
              "thing with taxon"
          )
          (
            "is_a"
            . "organismal entity"
          )
          (
            "subclass_of"
            . "NCBITaxon:1"
          )
          (
            "class_uri"
            . "SIO:010000"
          )
          (
            "mappings"
              "WD:Q795052"
              "UMLSSG:LIVB"
              "UMLSSC:T001"
              "UMLSST:orgm"
              "UMLSSC:T002"
              "UMLSST:plnt"
              "UMLSSC:T004"
              "UMLSST:fngs"
              "UMLSSC:T005"
              "UMLSST:virs"
              "UMLSSC:T007"
              "UMLSST:bact"
              "UMLSSC:T008"
              "UMLSST:anim"
              "UMLSSC:T010"
              "UMLSST:vtbt"
              "UMLSSC:T011"
              "UMLSST:amph"
              "UMLSSC:T012"
              "UMLSST:bird"
              "UMLSSC:T013"
              "UMLSST:fish"
              "UMLSSC:T014"
              "UMLSST:rept"
              "UMLSSC:T015"
              "UMLSST:mamm"
              "UMLSSC:T016"
              "UMLSST:humn"
              "UMLSSC:T096"
              "UMLSST:grup"
              "UMLSSC:T097"
              "UMLSST:prog"
              "UMLSSC:T099"
              "UMLSST:famg"
              "UMLSSC:T100"
              "UMLSST:aggp"
              "UMLSSC:T101"
              "UMLSST:podg"
              "UMLSSC:T194"
              "UMLSST:arch"
              "UMLSSC:T204"
              "UMLSST:euka"
          )
      )
      (
        "case"
          (
            "aliases"
              "patient"
              "proband"
          )
          (
            "is_a"
            . "individual organism"
          )
          (
            "description"
            . "An individual organism that has a patient role in some clinical context."
          )
      )
      (
        "population of individual organisms"
          (
            "description"
            . "A collection of individuals from the same taxonomic class distinguished by one or more characteristics. Characteristics can include, but are not limited to, shared geographic location, genetics, phenotypes [Alliance for Genome Resources]"
          )
          (
            "local_names"
              (
                "ga4gh"
                . "population"
              )
              (
                "agr"
                . "population"
              )
          )
          (
            "mixins"
              "thing with taxon"
          )
          (
            "is_a"
            . "organismal entity"
          )
          (
            "subclass_of"
            . "PCO:0000001"
          )
          (
            "class_uri"
            . "SIO:001061"
          )
          (
            "mappings"
              "UMLSSC:T098"
              "UMLSST:popg"
          )
          (
            "id_prefixes"
              "HANCESTRO"
          )
      )
      (
        "biosample"
          (
            "aliases"
              "biospecimen"
              "sample"
          )
          (
            "mixins"
              "thing with taxon"
          )
          (
            "is_a"
            . "organismal entity"
          )
          (
            "class_uri"
            . "SIO:001050"
          )
      )
      (
        "disease or phenotypic feature"
          (
            "aliases"
              "phenome"
          )
          (
            "is_a"
            . "biological entity"
          )
          (
            "description"
            . "Either one of a disease or an individual phenotypic feature. Some knowledge resources such as Monarch treat these as distinct, others such as MESH conflate."
          )
          (
            "mixins"
              "thing with taxon"
          )
          (
            "union_of"
              "disease"
              "phenotypic feature"
          )
          (
            "mappings"
              "UMLSSC:T033"
              "UMLSST:fndg"
          )
      )
      (
        "disease"
          (
            "aliases"
              "condition"
              "disorder"
              "medical condition"
          )
          (
            "is_a"
            . "disease or phenotypic feature"
          )
          (
            "class_uri"
            . "MONDO:0000001"
          )
          (
            "mappings"
              "WD:Q12136"
              "SIO:010299"
              "UMLSSG:DISO"
              "UMLSSC:T019"
              "UMLSST:cgab"
              "UMLSSC:T020"
              "UMLSST:acab"
              "UMLSSC:T037"
              "UMLSST:inpo"
              "UMLSSC:T046"
              "UMLSST:patf"
              "UMLSSC:T047"
              "UMLSST:dsyn"
              "UMLSSC:T048"
              "UMLSST:mobd"
              "UMLSSC:T049"
              "UMLSST:comd"
              "UMLSSC:T184"
              "UMLSST:sosy"
              "UMLSSC:T190"
              "UMLSST:anab"
              "UMLSSC:T191"
              "UMLSST:neop"
          )
      )
      (
        "phenotypic feature"
          (
            "aliases"
              "sign"
              "symptom"
              "phenotype"
              "trait"
              "endophenotype"
          )
          (
            "is_a"
            . "disease or phenotypic feature"
          )
          (
            "subclass_of"
            . "UPHENO:0001001"
          )
          (
            "class_uri"
            . "UPHENO:0001001"
          )
          (
            "mappings"
              "SIO:010056"
              "WD:Q169872"
          )
      )
      (
        "environment"
          (
            "aliases"
              "environment"
              "exposure"
              "experimental condition"
          )
          (
            "is_a"
            . "biological entity"
          )
          (
            "description"
            . "A feature of the environment of an organism that influences one or more phenotypic features of that organism, potentially mediated by genes"
          )
          (
            "class_uri"
            . "SIO:000955"
          )
      )
      (
        "information content entity"
          (
            "aliases"
              "information"
              "information artefact"
              "information entity"
          )
          (
            "abstract"
            . #t
          )
          (
            "is_a"
            . "named thing"
          )
          (
            "description"
            . "a piece of information that typically describes some piece of biology or is used as support."
          )
          (
            "class_uri"
            . "IAO:0000030"
          )
          (
            "mappings"
              "UMLSSG:CONC"
              "UMLSSC:T077"
              "UMLSST:cnce"
              "UMLSSC:T078"
              "UMLSST:idcn"
              "UMLSSC:T079"
              "UMLSST:tmco"
              "UMLSSC:T080"
              "UMLSST:qlco"
              "UMLSSC:T081"
              "UMLSST:qnco"
              "UMLSSC:T082"
              "UMLSST:spco"
              "UMLSSC:T089"
              "UMLSST:rnlw"
              "UMLSSC:T102"
              "UMLSST:grpa"
              "UMLSSC:T169"
              "UMLSST:ftcn"
              "UMLSSC:T171"
              "UMLSST:lang"
              "UMLSSC:T185"
              "UMLSST:clas"
          )
      )
      (
        "confidence level"
          (
            "is_a"
            . "information content entity"
          )
          (
            "description"
            . "Level of confidence in a statement"
          )
          (
            "values_from"
              "cio"
          )
          (
            "class_uri"
            . "CIO:0000028"
          )
      )
      (
        "evidence type"
          (
            "is_a"
            . "information content entity"
          )
          (
            "aliases"
              "evidence code"
          )
          (
            "description"
            . "Class of evidence that supports an association"
          )
          (
            "values_from"
              "eco"
          )
          (
            "class_uri"
            . "ECO:0000000"
          )
      )
      (
        "publication"
          (
            "is_a"
            . "information content entity"
          )
          (
            "aliases"
              "reference"
          )
          (
            "description"
            . "Any published piece of information. Can refer to a whole publication, or to a part of it (e.g. a figure, figure legend, or section highlighted by NLP). The scope is intended to be general and include information published on the web as well as journals."
          )
          (
            "class_uri"
            . "IAO:0000311"
          )
          (
            "mappings"
              "UMLSSC:T170"
              "UMLSST:inpr"
          )
          (
            "id_prefixes"
              "PMID"
          )
      )
      (
        "administrative entity"
          (
            "is_a"
            . "named thing"
          )
          (
            "abstract"
            . #t
          )
      )
      (
        "provider"
          (
            "is_a"
            . "administrative entity"
          )
          (
            "aliases"
              "agent"
              "group"
          )
          (
            "description"
            . "person, group, organization or project that provides a piece of information"
          )
          (
            "mappings"
              "UMLSSG:ORGA"
              "UMLSSC:T092"
              "UMLSST:orgt"
              "UMLSSC:T093"
              "UMLSST:hcro"
              "UMLSSC:T094"
              "UMLSST:pros"
              "UMLSSC:T095"
              "UMLSST:shro"
          )
      )
      (
        "molecular entity"
          (
            "is_a"
            . "biological entity"
          )
          (
            "mixins"
              "thing with taxon"
          )
          (
            "aliases"
              "bioentity"
          )
          (
            "description"
            . "A gene, gene product, small molecule or macromolecule (including protein complex)"
          )
          (
            "class_uri"
            . "SIO:010341"
          )
          (
            "mappings"
              "WD:Q43460564"
              "UMLSSG:GENE"
              "UMLSSC:T085"
              "UMLSST:mosq"
          )
      )
      (
        "chemical substance"
          (
            "is_a"
            . "molecular entity"
          )
          (
            "description"
            . "May be a chemical entity or a formulation with a chemical entity as active ingredient, or a complex material with multiple chemical entities as part"
          )
          (
            "subclass_of"
            . "CHEBI:24431"
          )
          (
            "class_uri"
            . "SIO:010004"
          )
          (
            "mappings"
              "WD:Q79529"
              "UMLSSC:T167"
              "UMLSST:sbst"
              "UMLSSG:CHEM"
              "UMLSSC:T103"
              "UMLSST:chem"
              "UMLSSC:T104"
              "UMLSST:chvs"
              "UMLSSC:T109"
              "UMLSST:orch"
              "UMLSSC:T114"
              "UMLSST:nnon"
              "UMLSSC:T120"
              "UMLSST:chvf"
              "UMLSSC:T121"
              "UMLSST:phsu"
              "UMLSSC:T122"
              "UMLSST:bodm"
              "UMLSSC:T123"
              "UMLSST:bacs"
              "UMLSSC:T125"
              "UMLSST:horm"
              "UMLSSC:T126"
              "UMLSST:enzy"
              "UMLSSC:T127"
              "UMLSST:vita"
              "UMLSSC:T129"
              "UMLSST:imft"
              "UMLSSC:T130"
              "UMLSST:irda"
              "UMLSSC:T131"
              "UMLSST:hops"
              "UMLSSC:T192"
              "UMLSST:rcpt"
              "UMLSSC:T195"
              "UMLSST:antb"
              "UMLSSC:T196"
              "UMLSST:elii"
              "UMLSSC:T197"
              "UMLSST:inch"
          )
          (
            "id_prefixes"
              "CHEBI"
              "CHEMBL.COMPOUND"
          )
      )
      (
        "carbohydrate"
          (
            "is_a"
            . "chemical substance"
          )
          (
            "mappings"
              "UMLSSC:T088"
              "UMLSST:crbs"
          )
      )
      (
        "drug"
          (
            "is_a"
            . "chemical substance"
          )
          (
            "description"
            . "A substance intended for use in the diagnosis, cure, mitigation, treatment, or prevention of disease"
          )
          (
            "comments"
              "The CHEBI ID represents a role rather than a substance"
          )
          (
            "class_uri"
            . "WD:Q12140"
          )
          (
            "mappings"
              "CHEBI:23888"
              "UMLSSC:T200"
              "UMLSST:clnd"
          )
      )
      (
        "metabolite"
          (
            "is_a"
            . "chemical substance"
          )
          (
            "description"
            . "Any intermediate or product resulting from metabolism. Includes primary and secondary metabolites."
          )
          (
            "comments"
              "The CHEBI ID represents a role rather than a substance"
          )
          (
            "class_uri"
            . "CHEBI:25212"
          )
      )
      (
        "anatomical entity"
          (
            "is_a"
            . "organismal entity"
          )
          (
            "mixins"
              "thing with taxon"
          )
          (
            "subclass_of"
            . "UBERON:0001062"
          )
          (
            "description"
            . "A subcellular location, cell type or gross anatomical part"
          )
          (
            "class_uri"
            . "SIO:010046"
          )
          (
            "mappings"
              "WD:Q4936952"
              "UMLSSG:ANAT"
              "UMLSSC:T022"
              "UMLSST:bdsy"
              "UMLSSC:T029"
              "UMLSST:blor"
              "UMLSSC:T030"
              "UMLSST:bsoj"
              "UMLSSC:T031"
              "UMLSST:bdsu"
          )
      )
      (
        "life stage"
          (
            "is_a"
            . "organismal entity"
          )
          (
            "mixins"
              "thing with taxon"
          )
          (
            "subclass_of"
            . "UBERON:0000105"
          )
          (
            "description"
            . "A stage of development or growth of an organism, including post-natal adult stages"
          )
      )
      (
        "planetary entity"
          (
            "is_a"
            . "named thing"
          )
          (
            "description"
            . "Any entity or process that exists at the level of the whole planet"
          )
      )
      (
        "environmental process"
          (
            "is_a"
            . "planetary entity"
          )
          (
            "mixins"
              "occurrent"
          )
          (
            "subclass_of"
            . "ENVO:02500000"
          )
      )
      (
        "environmental feature"
          (
            "is_a"
            . "planetary entity"
          )
          (
            "subclass_of"
            . "ENVO:00002297"
          )
      )
      (
        "clinical entity"
          (
            "is_a"
            . "named thing"
          )
          (
            "description"
            . "Any entity or process that exists in the clinical domain and outside the biological realm. Diseases are placed under biological entities"
          )
      )
      (
        "clinical trial"
          (
            "is_a"
            . "clinical entity"
          )
      )
      (
        "clinical intervention"
          (
            "is_a"
            . "clinical entity"
          )
      )
      (
        "device"
          (
            "is_a"
            . "named thing"
          )
          (
            "description"
            . "A thing made or adapted for a particular purpose, especially a piece of mechanical or electronic equipment"
          )
          (
            "mappings"
              "UMLSSG:DEVI"
              "UMLSSC:T074"
              "UMLSST:medd"
              "UMLSSC:T075"
              "UMLSST:resd"
              "UMLSSC:T203"
              "UMLSST:drdd"
          )
      )
      (
        "genomic entity"
          (
            "is_a"
            . "molecular entity"
          )
          (
            "aliases"
              "sequence feature"
          )
          (
            "description"
            . "an entity that can either be directly located on a genome (gene, transcript, exon, regulatory region) or is encoded in a genome (protein)"
          )
          (
            "slots"
              "has biological sequence"
          )
          (
            "class_uri"
            . "SO:0000110"
          )
          (
            "mappings"
              "UMLSSC:T028"
              "UMLSST:gngm"
              "UMLSSC:T086"
              "UMLSST:nusq"
          )
      )
      (
        "genome"
          (
            "is_a"
            . "genomic entity"
          )
          (
            "description"
            . "A genome is the sum of genetic material within a cell or virion."
          )
          (
            "class_uri"
            . "SO:0001026"
          )
          (
            "mappings"
              "SIO:000984"
              "WD:Q7020"
          )
      )
      (
        "transcript"
          (
            "is_a"
            . "genomic entity"
          )
          (
            "description"
            . "An RNA synthesized on a DNA or RNA template by an RNA polymerase"
          )
          (
            "class_uri"
            . "SO:0000673"
          )
          (
            "mappings"
              "SIO:010450"
          )
      )
      (
        "exon"
          (
            "is_a"
            . "genomic entity"
          )
          (
            "description"
            . "A region of the transcript sequence within a gene which is not removed from the primary RNA transcript by RNA splicing"
          )
          (
            "class_uri"
            . "SO:0000147"
          )
          (
            "mappings"
              "SIO:010445"
              "WD:Q373027"
          )
      )
      (
        "coding sequence"
          (
            "is_a"
            . "genomic entity"
          )
          (
            "class_uri"
            . "SO:0000316"
          )
          (
            "mappings"
              "SIO:001390"
          )
      )
      (
        "macromolecular machine"
          (
            "is_a"
            . "genomic entity"
          )
          (
            "union_of"
              "gene"
              "gene product"
              "macromolecular complex"
          )
          (
            "description"
            . "A union of gene, gene product, and macromolecular complex. These are the basic units of function in a cell. They either carry out individual biological activities, or they encode molecules which do this."
          )
          (
            "slot_usage"
              (
                "name"
                  (
                    "range"
                    . "symbol type"
                  )
                  (
                    "description"
                    . "genes are typically designated by a short symbol and a full name. We map the symbol to the default display name and use an additional slot for full name"
                  )
              )
          )
      )
      (
        "gene or gene product"
          (
            "is_a"
            . "macromolecular machine"
          )
          (
            "union_of"
              "gene"
              "gene product"
          )
          (
            "description"
            . "a union of genes or gene products. Frequently an identifier for one will be used as proxy for another"
          )
          (
            "id_prefixes"
              "CHEMBL.TARGET"
          )
      )
      (
        "gene"
          (
            "is_a"
            . "gene or gene product"
          )
          (
            "aliases"
              "locus"
          )
          (
            "class_uri"
            . "SO:0000704"
          )
          (
            "mappings"
              "SIO:010035"
              "WD:Q7187"
          )
          (
            "id_prefixes"
              "NCBIGene"
              "ENSEMBL"
              "HGNC"
              "MGI"
              "ZFIN"
              "dictyBase"
              "WB"
              "SGD"
              "PomBase"
          )
      )
      (
        "gene product"
          (
            "is_a"
            . "gene or gene product"
          )
          (
            "description"
            . "The functional molecular product of a single gene. Gene products are either proteins or functional RNA molecules"
          )
          (
            "union_of"
              "protein"
              "RNA product"
          )
          (
            "class_uri"
            . "WD:Q424689"
          )
      )
      (
        "protein"
          (
            "is_a"
            . "gene product"
          )
          (
            "aliases"
              "polypeptide"
          )
          (
            "description"
            . "A gene product that is composed of a chain of amino acid sequences and is produced by ribosome-mediated translation of mRNA"
          )
          (
            "class_uri"
            . "PR:000000001"
          )
          (
            "mappings"
              "SIO:010043"
              "WD:Q8054"
              "UMLSSC:T087"
              "UMLSST:amas"
              "UMLSSC:T116"
              "UMLSST:aapp"
          )
          (
            "id_prefixes"
              "UniProtKB"
              "PR"
              "ENSEMBL"
          )
      )
      (
        "gene product isoform"
          (
            "is_a"
            . "gene product"
          )
          (
            "description"
            . "This is an abstract class that can be mixed in with different kinds of gene products to indicate that the gene product is intended to represent a specific isoform rather than a canonical or reference or generic product. The designation of canonical or reference may be arbitrary, or it may represent the superclass of all isoforms."
          )
          (
            "abstract"
            . #t
          )
      )
      (
        "protein isoform"
          (
            "aliases"
              "proteoform"
          )
          (
            "is_a"
            . "protein"
          )
          (
            "description"
            . "Represents a protein that is a specific isoform of the canonical or reference protein. See https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4114032/"
          )
          (
            "mixins"
              "gene product isoform"
          )
          (
            "id_prefixes"
              "UniProtKB"
              "PR"
              "ENSEMBL"
          )
      )
      (
        "RNA product"
          (
            "is_a"
            . "gene product"
          )
          (
            "class_uri"
            . "CHEBI:33697"
          )
          (
            "mappings"
              "SIO:010450"
              "WD:Q11053"
          )
          (
            "id_prefixes"
              "RNAcentral"
          )
      )
      (
        "RNA product isoform"
          (
            "is_a"
            . "RNA product"
          )
          (
            "description"
            . "Represents a protein that is a specific isoform of the canonical or reference RNA"
          )
          (
            "mixins"
              "gene product isoform"
          )
          (
            "id_prefixes"
              "RNAcentral"
          )
      )
      (
        "noncoding RNA product"
          (
            "is_a"
            . "RNA product"
          )
          (
            "id_prefixes"
              "RNAcentral"
              "NCBIGene"
              "ENSEMBL"
          )
          (
            "subclass_of"
            . "SO:0000655"
          )
          (
            "class_uri"
            . "SIO:001235"
          )
      )
      (
        "microRNA"
          (
            "is_a"
            . "noncoding RNA product"
          )
          (
            "subclass_of"
            . "SO:0000276"
          )
          (
            "class_uri"
            . "SIO:001397"
          )
          (
            "mappings"
              "WD:Q310899"
          )
          (
            "id_prefixes"
              "MIR"
          )
      )
      (
        "macromolecular complex"
          (
            "is_a"
            . "macromolecular machine"
          )
          (
            "subclass_of"
            . "GO:0032991"
          )
          (
            "class_uri"
            . "SIO:010046"
          )
          (
            "mappings"
              "WD:Q22325163"
          )
          (
            "id_prefixes"
              "IntAct"
              "GO"
              "PR"
              "Reactome"
          )
      )
      (
        "gene grouping"
          (
            "abstract"
            . #t
          )
          (
            "mixin"
            . #t
          )
          (
            "description"
            . "any grouping of multiple genes or gene products"
          )
      )
      (
        "gene family"
          (
            "is_a"
            . "molecular entity"
          )
          (
            "class_uri"
            . "SIO:001380"
          )
          (
            "mappings"
              "NCIT:C20130"
              "WD:Q417841"
          )
          (
            "mixins"
              "gene grouping"
          )
          (
            "description"
            . "any grouping of multiple genes or gene products related by common descent"
          )
          (
            "id_prefixes"
              "PANTHER"
          )
      )
      (
        "zygosity"
          (
            "is_a"
            . "attribute"
          )
          (
            "class_uri"
            . "GENO:0000133"
          )
      )
      (
        "genotype"
          (
            "is_a"
            . "genomic entity"
          )
          (
            "description"
            . "An information content entity that describes a genome by specifying the total variation in genomic sequence and/or gene expression, relative to some extablished background"
          )
          (
            "comments"
              "Consider renaming as genotypic entity"
          )
          (
            "slots"
              "has zygosity"
          )
          (
            "class_uri"
            . "GENO:0000536"
          )
          (
            "mappings"
              "SIO:001079"
          )
      )
      (
        "haplotype"
          (
            "is_a"
            . "genomic entity"
          )
          (
            "description"
            . "A set of zero or more Alleles on a single instance of a Sequence[VMC]"
          )
          (
            "class_uri"
            . "GENO:0000871"
          )
          (
            "mappings"
              "VMC:Haplotype"
          )
      )
      (
        "sequence variant"
          (
            "aliases"
              "allele"
          )
          (
            "local_names"
              (
                "agr"
                . "allele"
              )
          )
          (
            "is_a"
            . "genomic entity"
          )
          (
            "description"
            . "An allele that varies in its sequence from what is considered the reference allele at that locus."
          )
          (
            "comments"
              "This class is for modeling the specific state at a locus. A single dbSNP rs ID could correspond to more than one sequence variants (e.g CIViC:1252 and CIViC:1253, two distinct BRCA2 alleles for rs28897743)"
          )
          (
            "class_uri"
            . "GENO:0000002"
          )
          (
            "mappings"
              "WD:Q15304597"
              "SIO:010277"
              "VMC:Allele"
          )
          (
            "id_prefixes"
              "ClinVar"
              "WD"
              "CIViC"
          )
          (
            "alt_descriptions"
              (
                "AGR"
                . "An enitity that describes a single affected, endogenous allele.  These can be of any type that matches that definition"
              )
              (
                "VMC"
                . "A contiguous change at a Location"
              )
          )
          (
            "slots"
              "has gene"
          )
          (
            "slot_usage"
              (
                "has gene"
                  (
                    "multivalued"
                    . #t
                  )
                  (
                    "description"
                    . "Each allele can be associated with any number of genes"
                  )
              )
              (
                "has biological sequence"
                  (
                    "description"
                    . "The state of the sequence w.r.t a reference sequence"
                  )
              )
              (
                "id"
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "ZFIN:ZDB-ALT-980203-1091"
                        )
                        (
                          "description"
                          . "ti282a allele from ZFIN"
                        )
                      )
                      (
                        (
                          "value"
                          . "ClinVarVariant:17681"
                        )
                        (
                          "description"
                          . "NM_007294.3(BRCA1):c.2521C>T (p.Arg841Trp)"
                        )
                      )
                  )
              )
          )
      )
      (
        "drug exposure"
          (
            "aliases"
              "drug intake"
              "drug dose"
          )
          (
            "is_a"
            . "environment"
          )
          (
            "description"
            . "A drug exposure is an intake of a particular chemical substance"
          )
          (
            "slot_usage"
              (
                "drug"
                  (
                    "range"
                    . "chemical substance"
                  )
                  (
                    "required"
                    . #t
                  )
                  (
                    "multivalued"
                    . #t
                  )
              )
          )
          (
            "class_uri"
            . "ECTO:0000509"
          )
          (
            "mappings"
              "SIO:001005"
          )
      )
      (
        "treatment"
          (
            "aliases"
              "medical action"
          )
          (
            "is_a"
            . "environment"
          )
          (
            "description"
            . "A treatment is targeted at a disease or phenotype and may involve multiple drug 'exposures'"
          )
          (
            "slot_usage"
              (
                "has exposure parts"
                  (
                    "multivalued"
                    . #t
                  )
                  (
                    "range"
                    . "drug exposure"
                  )
                  (
                    "required"
                    . #t
                  )
              )
          )
          (
            "class_uri"
            . "OGMS:0000090"
          )
          (
            "mappings"
              "SIO:001398"
          )
      )
      (
        "geographic location"
          (
            "is_a"
            . "planetary entity"
          )
          (
            "description"
            . "a location that can be described in lat/long coordinates"
          )
          (
            "slots"
              "latitude"
              "longitude"
          )
          (
            "mappings"
              "UMLSSG:GEOG"
              "UMLSST:geoa"
              "UMLSSC:T083"
          )
      )
      (
        "geographic location at time"
          (
            "is_a"
            . "geographic location"
          )
          (
            "description"
            . "a location that can be described in lat/long coordinates, for a particular time"
          )
          (
            "slots"
              "timepoint"
          )
      )
      (
        "association"
          (
            "description"
            . "A typed association between two entities, supported by evidence"
          )
          (
            "comments"
              "This is roughly the model used by biolink and ontobio at the moment"
          )
          (
            "slots"
              "subject"
              "relation"
              "object"
              "association_id"
              "negated"
              "association type"
              "qualifiers"
              "publications"
              "provided by"
          )
          (
            "class_uri"
            . "OBAN:association"
          )
          (
            "mappings"
              "rdf:Statement"
              "owl:Axiom"
          )
      )
      (
        "genotype to genotype part association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "description"
            . "Any association between one genotype and a genotypic entity that is a sub-component of it"
          )
          (
            "slot_usage"
              (
                "relation"
                  (
                    "subproperty_of"
                    . "has variant part"
                  )
              )
              (
                "subject"
                  (
                    "range"
                    . "genotype"
                  )
                  (
                    "description"
                    . "parent genotype"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "genotype"
                  )
                  (
                    "description"
                    . "child genotype"
                  )
              )
          )
      )
      (
        "genotype to gene association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "description"
            . "Any association between a genotype and a gene. The genotype have have multiple variants in that gene or a single one. There is no assumption of cardinality"
          )
          (
            "slot_usage"
              (
                "relation"
                  (
                    "description"
                    . "the relationship type used to connect genotype to gene"
                  )
              )
              (
                "subject"
                  (
                    "range"
                    . "genotype"
                  )
                  (
                    "description"
                    . "parent genotype"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "gene"
                  )
                  (
                    "description"
                    . "gene implicated in genotype"
                  )
              )
          )
      )
      (
        "genotype to variant association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "description"
            . "Any association between a genotype and a sequence variant."
          )
          (
            "slot_usage"
              (
                "relation"
                  (
                    "description"
                    . "the relationship type used to connect genotype to gene"
                  )
              )
              (
                "subject"
                  (
                    "range"
                    . "genotype"
                  )
                  (
                    "description"
                    . "parent genotype"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "sequence variant"
                  )
                  (
                    "description"
                    . "gene implicated in genotype"
                  )
              )
          )
      )
      (
        "gene to gene association"
          (
            "aliases"
              "molecular or genetic interaction"
          )
          (
            "abstract"
            . #t
          )
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "description"
            . "abstract parent class for different kinds of gene-gene or gene product to gene product relationships. Includes homology and interaction."
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "gene or gene product"
                  )
                  (
                    "description"
                    . "the subject gene in the association. If the relation is symmetric, subject vs object is arbitrary. We allow a gene product to stand as proxy for the gene or vice versa"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "gene or gene product"
                  )
                  (
                    "description"
                    . "the object gene in the association. If the relation is symmetric, subject vs object is arbitrary. We allow a gene product to stand as proxy for the gene or vice versa"
                  )
              )
          )
      )
      (
        "gene to gene homology association"
          (
            "is_a"
            . "gene to gene association"
          )
          (
            "defining_slots"
              "subject"
              "object"
              "relation"
          )
          (
            "description"
            . "A homology association between two genes. May be orthology (in which case the species of subject and object should differ) or paralogy (in which case the species may be the same)"
          )
          (
            "slot_usage"
              (
                "relation"
                  (
                    "subproperty_of"
                    . "homologous to"
                  )
                  (
                    "symmetric"
                    . #t
                  )
                  (
                    "description"
                    . "homology relationship type"
                  )
              )
          )
      )
      (
        "pairwise interaction association"
          (
            "is_a"
            . "association"
          )
          (
            "mixin"
            . #t
          )
          (
            "abstract"
            . #t
          )
          (
            "description"
            . "An interaction at the molecular level between two physical entities"
          )
          (
            "defining_slots"
              "subject"
              "object"
              "relation"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "molecular entity"
                  )
              )
              (
                "id"
                  (
                    "description"
                    . "identifier for the interaction. This may come from an interaction database such as IMEX."
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "WB:WBInteraction000538741"
                        )
                      )
                  )
                  (
                    "values_from"
                      "IMEX"
                      "BioGRID"
                  )
              )
              (
                "relation"
                  (
                    "subproperty_of"
                    . "molecularly interacts with"
                  )
                  (
                    "values_from"
                      "ro"
                      "mi"
                  )
                  (
                    "description"
                    . "interaction relationship type"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "RO:0002447"
                        )
                        (
                          "description"
                          . "the subject molecular phosphorylates the object molecule"
                        )
                      )
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "molecular entity"
                  )
              )
              (
                "interacting molecules category"
                  (
                    "range"
                    . "ontology class"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "MI:1048"
                        )
                        (
                          "description"
                          . "smallmolecule-protein"
                        )
                      )
                  )
              )
          )
      )
      (
        "pairwise gene to gene interaction"
          (
            "is_a"
            . "gene to gene association"
          )
          (
            "defining_slots"
              "subject"
              "object"
              "relation"
          )
          (
            "mixins"
              "pairwise interaction association"
          )
          (
            "description"
            . "An interaction between two genes or two gene products. May be physical (e.g. protein binding) or genetic (between genes). May be symmetric (e.g. protein interaction) or directed (e.g. phosphorylation)"
          )
          (
            "slot_usage"
              (
                "relation"
                  (
                    "subproperty_of"
                    . "molecularly interacts with"
                  )
                  (
                    "symmetric"
                    . #t
                  )
                  (
                    "values_from"
                      "ro"
                      "mi"
                  )
                  (
                    "description"
                    . "interaction relationship type"
                  )
              )
          )
      )
      (
        "cell line to thing association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
          )
          (
            "abstract"
            . #t
          )
          (
            "description"
            . "An relationship between a cell line and another entity"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "cell line"
                  )
              )
          )
      )
      (
        "cell line to disease or phenotypic feature association"
          (
            "is_a"
            . "association"
          )
          (
            "mixins"
              "cell line to thing association"
              "thing to disease or phenotypic feature association"
          )
          (
            "description"
            . "An relationship between a cell line and a disease or a phenotype, where the cell line is derived from an individual with that disease or phenotype"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "disease or phenotypic feature"
                  )
              )
          )
      )
      (
        "chemical to thing association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
          )
          (
            "abstract"
            . #t
          )
          (
            "description"
            . "An interaction between a chemical entity and another entity"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "chemical substance"
                  )
                  (
                    "description"
                    . "the chemical substance or entity that is an interactor"
                  )
              )
          )
      )
      (
        "case to thing association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
          )
          (
            "abstract"
            . #t
          )
          (
            "description"
            . "An abstract association for use where the case is the subject"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "case"
                  )
                  (
                    "description"
                    . "the case (e.g. patient) that has the property"
                  )
              )
          )
      )
      (
        "chemical to disease or phenotypic feature association"
          (
            "is_a"
            . "association"
          )
          (
            "class_uri"
            . "SIO:000993"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "mixins"
              "chemical to thing association"
              "thing to disease or phenotypic feature association"
          )
          (
            "description"
            . "An interaction between a chemical entity and a phenotype or disease, where the presence of the chemical gives rise to or exacerbates the phenotype"
          )
          (
            "slot_usage"
              (
                "object"
                  (
                    "range"
                    . "disease or phenotypic feature"
                  )
                  (
                    "description"
                    . "the disease or phenotype that is affected by the chemical"
                  )
              )
          )
      )
      (
        "chemical to pathway association"
          (
            "is_a"
            . "association"
          )
          (
            "class_uri"
            . "SIO:001250"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "mixins"
              "chemical to thing association"
          )
          (
            "description"
            . "An interaction between a chemical entity and a biological process or pathway"
          )
          (
            "slot_usage"
              (
                "object"
                  (
                    "range"
                    . "pathway"
                  )
                  (
                    "description"
                    . "the pathway that is affected by the chemical"
                  )
              )
          )
      )
      (
        "chemical to gene association"
          (
            "is_a"
            . "association"
          )
          (
            "class_uri"
            . "SIO:001257"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "mixins"
              "chemical to thing association"
          )
          (
            "description"
            . "An interaction between a chemical entity and a gene or gene product"
          )
          (
            "slot_usage"
              (
                "object"
                  (
                    "range"
                    . "gene or gene product"
                  )
                  (
                    "description"
                    . "the gene or gene product that is affected by the chemical"
                  )
              )
          )
      )
      (
        "biosample to thing association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
          )
          (
            "abstract"
            . #t
          )
          (
            "description"
            . "An association between a biosample and something"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "biosample"
                  )
                  (
                    "description"
                    . "the biosample being described"
                  )
              )
          )
      )
      (
        "biosample to disease or phenotypic feature association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "mixins"
              "biosample to thing association"
              "thing to disease or phenotypic feature association"
          )
          (
            "description"
            . "An association between a biosample and a disease or phenotype"
          )
      )
      (
        "frequency qualifier mixin"
          (
            "mixin"
            . #t
          )
          (
            "description"
            . "Qualifier for freqency type associations"
          )
          (
            "slots"
              "frequency qualifier"
          )
      )
      (
        "entity to feature or disease qualifiers"
          (
            "abstract"
            . #t
          )
          (
            "mixin"
            . #t
          )
          (
            "is_a"
            . "frequency qualifier mixin"
          )
          (
            "description"
            . "Qualifiers for entity to disease or phenotype associations"
          )
          (
            "slots"
              "severity qualifier"
              "onset qualifier"
          )
      )
      (
        "entity to phenotypic feature association"
          (
            "abstract"
            . #t
          )
          (
            "defining_slots"
              "object"
          )
          (
            "is_a"
            . "association"
          )
          (
            "mixins"
              "entity to feature or disease qualifiers"
          )
          (
            "slot_usage"
              (
                "description"
                  (
                    "description"
                    . "A description of specific aspects of this phenotype, not otherwise covered by the phenotype ontology class"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "phenotypic feature"
                  )
                  (
                    "description"
                    . "phenotypic class"
                  )
                  (
                    "values_from"
                      "upheno"
                      "hp"
                      "mp"
                      "wbphenotypic feature"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "HP:0002487"
                        )
                        (
                          "description"
                          . "Hyperkinesis"
                        )
                      )
                      (
                        (
                          "value"
                          . "WBPhenotype:0000180"
                        )
                        (
                          "description"
                          . "axon morphology variant"
                        )
                      )
                      (
                        (
                          "value"
                          . "MP:0001569"
                        )
                        (
                          "description"
                          . "abnormal circulating bilirubin level"
                        )
                      )
                  )
              )
          )
          (
            "slots"
              "sex qualifier"
          )
      )
      (
        "entity to disease association"
          (
            "description"
            . "mixin class for any association whose object (target node) is a disease"
          )
          (
            "abstract"
            . #t
          )
          (
            "mixin"
            . #t
          )
          (
            "is_a"
            . "entity to feature or disease qualifiers"
          )
          (
            "defining_slots"
              "object"
          )
          (
            "slot_usage"
              (
                "object"
                  (
                    "range"
                    . "disease"
                  )
                  (
                    "description"
                    . "disease"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "MONDO:0020066"
                        )
                        (
                          "description"
                          . "Ehlers-Danlos syndrome"
                        )
                      )
                  )
              )
          )
      )
      (
        "disease or phenotypic feature association to thing association"
          (
            "abstract"
            . #t
          )
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "disease or phenotypic feature"
                  )
                  (
                    "description"
                    . "disease or phenotype"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "MONDO:0017314"
                        )
                        (
                          "description"
                          . "Ehlers-Danlos syndrome, vascular type"
                        )
                      )
                      (
                        (
                          "value"
                          . "MP:0013229"
                        )
                        (
                          "description"
                          . "abnormal brain ventricle size"
                        )
                      )
                  )
              )
          )
      )
      (
        "disease or phenotypic feature association to location association"
          (
            "is_a"
            . "disease or phenotypic feature association to thing association"
          )
          (
            "description"
            . "An association between either a disease or a phenotypic feature and an anatomical entity, where the disease/feature manifests in that site."
          )
          (
            "class_uri"
            . "NCIT:R100"
          )
          (
            "slot_usage"
              (
                "object"
                  (
                    "range"
                    . "anatomical entity"
                  )
                  (
                    "description"
                    . "anatomical entity in which the disease or feature is found"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "UBERON:0002048"
                        )
                        (
                          "description"
                          . "lung"
                        )
                      )
                  )
              )
          )
      )
      (
        "thing to disease or phenotypic feature association"
          (
            "abstract"
            . #t
          )
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "object"
          )
          (
            "slot_usage"
              (
                "object"
                  (
                    "range"
                    . "disease or phenotypic feature"
                  )
                  (
                    "description"
                    . "disease or phenotype"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "MONDO:0017314"
                        )
                        (
                          "description"
                          . "Ehlers-Danlos syndrome, vascular type"
                        )
                      )
                      (
                        (
                          "value"
                          . "MP:0013229"
                        )
                        (
                          "description"
                          . "abnormal brain ventricle size"
                        )
                      )
                  )
              )
          )
      )
      (
        "disease to thing association"
          (
            "abstract"
            . #t
          )
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "disease"
                  )
                  (
                    "description"
                    . "disease class"
                  )
                  (
                    "values_from"
                      "mondo"
                      "omim"
                      "orphanet"
                      "ncit"
                      "doid"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "MONDO:0017314"
                        )
                        (
                          "description"
                          . "Ehlers-Danlos syndrome, vascular type"
                        )
                      )
                  )
              )
          )
      )
      (
        "genotype to phenotypic feature association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "description"
            . "Any association between one genotype and a phenotypic feature, where having the genotype confers the phenotype, either in isolation or through environment"
          )
          (
            "mixins"
              "entity to phenotypic feature association"
              "genotype to thing association"
          )
          (
            "slot_usage"
              (
                "relation"
                  (
                    "subproperty_of"
                    . "has phenotype"
                  )
              )
              (
                "subject"
                  (
                    "range"
                    . "genotype"
                  )
                  (
                    "description"
                    . "genotype that is associated with the phenotypic feature"
                  )
              )
          )
      )
      (
        "environment to phenotypic feature association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "description"
            . "Any association between an environment and a phenotypic feature, where being in the environment influences the phenotype"
          )
          (
            "mixins"
              "entity to phenotypic feature association"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "environment"
                  )
              )
          )
      )
      (
        "disease to phenotypic feature association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "description"
            . "An association between a disease and a phenotypic feature in which the phenotypic feature is associated with the disease in some way"
          )
          (
            "mixins"
              "entity to phenotypic feature association"
              "disease to thing association"
          )
      )
      (
        "case to phenotypic feature association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "description"
            . "An association between a case (e.g. individual patient) and a phenotypic feature in which the individual has or has had the phenotype"
          )
          (
            "mixins"
              "entity to phenotypic feature association"
              "case to thing association"
          )
      )
      (
        "gene to thing association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
          )
          (
            "abstract"
            . #t
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "gene or gene product"
                  )
                  (
                    "description"
                    . "gene that is the subject of the association"
                  )
              )
          )
      )
      (
        "variant to thing association"
          (
            "is_a"
            . "association"
          )
          (
            "local_names"
              (
                "ga4gh"
                . "variant annotation"
              )
          )
          (
            "defining_slots"
              "subject"
          )
          (
            "abstract"
            . #t
          )
          (
            "mixin"
            . #t
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "sequence variant"
                  )
                  (
                    "description"
                    . "a sequence variant in which the allele state is associated with some other entity"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "ClinVar:38077"
                        )
                        (
                          "description"
                          . "ClinVar representation of NM_000059.3(BRCA2):c.7007G>A (p.Arg2336His)"
                        )
                      )
                      (
                        (
                          "value"
                          . "ClinGen:CA024716"
                        )
                        (
                          "description"
                          . "chr13:g.32921033G>C (hg19) in ClinGen"
                        )
                      )
                  )
              )
          )
      )
      (
        "gene to phenotypic feature association"
          (
            "is_a"
            . "association"
          )
          (
            "class_uri"
            . "http://bio2rdf.org/wormbase_vocabulary:Gene-Phenotype-Association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "mixins"
              "entity to phenotypic feature association"
              "gene to thing association"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "gene or gene product"
                  )
                  (
                    "description"
                    . "gene in which variation is correlated with the phenotypic feature"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "HGNC:2197"
                        )
                        (
                          "description"
                          . "COL1A1 (Human)"
                        )
                      )
                  )
              )
          )
      )
      (
        "gene to disease association"
          (
            "is_a"
            . "association"
          )
          (
            "comments"
              "NCIT:R176 refers to the inverse relationship"
          )
          (
            "class_uri"
            . "SIO:000983"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "mixins"
              "entity to disease association"
              "gene to thing association"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "gene or gene product"
                  )
                  (
                    "description"
                    . "gene in which variation is correlated with the disease - may be protective or causative or associative, or as a model"
                  )
              )
          )
      )
      (
        "variant to population association"
          (
            "description"
            . "An association between a variant and a population, where the variant has particular frequency in the population"
          )
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "mixins"
              "variant to thing association"
              "frequency quantifier"
              "frequency qualifier mixin"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "sequence variant"
                  )
                  (
                    "description"
                    . "an allele that has a certain frequency in a given population"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "NC_000017.11:g.43051071A>T"
                        )
                        (
                          "description"
                          . "17:41203088 A/C in gnomad"
                        )
                      )
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "population of individual organisms"
                  )
                  (
                    "description"
                    . "the population that is observed to have the frequency"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "HANCESTRO:0010"
                        )
                        (
                          "description"
                          . "African"
                        )
                      )
                  )
              )
              (
                "has quotient"
                  (
                    "description"
                    . "frequency of allele in population, expressed as a number with allele divided by number in reference population, aka allele frequency"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "0.0001666"
                        )
                      )
                  )
              )
              (
                "has count"
                  (
                    "description"
                    . "number in object population that carry a particular allele, aka allele count"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "4"
                        )
                        (
                          "description"
                          . "4 individuals in gnomad set"
                        )
                      )
                  )
              )
              (
                "has total"
                  (
                    "description"
                    . "number all populations that carry a particular allele, aka allele number"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "24014"
                        )
                        (
                          "description"
                          . "24014 individuals in gnomad set"
                        )
                      )
                  )
              )
          )
      )
      (
        "population to population association"
          (
            "description"
            . "An association between a two populations"
          )
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "population of individual organisms"
                  )
                  (
                    "description"
                    . "the population that form the subject of the association"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "population of individual organisms"
                  )
                  (
                    "description"
                    . "the population that form the object of the association"
                  )
              )
              (
                "relation"
                  (
                    "description"
                    . "A relationship type that holds between the subject and object populations. Standard mereological relations can be used. E.g. subject part-of object, subject overlaps object. Derivation relationships can also be used"
                  )
              )
          )
      )
      (
        "variant to phenotypic feature association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "mixins"
              "variant to thing association"
              "entity to phenotypic feature association"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "sequence variant"
                  )
                  (
                    "description"
                    . "a sequence variant in which the allele state is associated in some way with the phenotype state"
                  )
              )
          )
      )
      (
        "variant to disease association"
          (
            "is_a"
            . "association"
          )
          (
            "comments"
              "TODO decide no how to model pathogenicity"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "mixins"
              "variant to thing association"
              "entity to disease association"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "description"
                    . "a sequence variant in which the allele state is associated in some way with the disease state"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "ClinVar:52241"
                        )
                        (
                          "description"
                          . "NM_000059.3(BRCA2):c.7007G>C (p.Arg2336Pro)"
                        )
                      )
                  )
              )
              (
                "relation"
                  (
                    "description"
                    . "E.g. is pathogenic for"
                  )
                  (
                    "subproperty_of"
                    . "related condition"
                  )
              )
              (
                "object"
                  (
                    "description"
                    . "a disease that is associated with that variant"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "MONDO:0016419"
                        )
                        (
                          "description"
                          . "hereditary breast cancer"
                        )
                      )
                  )
              )
          )
      )
      (
        "model to disease mixin"
          (
            "mixin"
            . #t
          )
          (
            "abstract"
            . #t
          )
          (
            "description"
            . "This mixin is used for any association class for which the subject (source node) plays the role of a 'model', in that it recapitulates some features of the disease in a way that is useful for studying the disease outside a patient carrying the disease"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "description"
                    . "The entity that serves as the model of the disease. This may be an organism, a strain of organism, a genotype or variant that exhibits similar features, or a gene that when mutated exhibits features of the disease"
                  )
              )
              (
                "relation"
                  (
                    "subproperty_of"
                    . "model of"
                  )
                  (
                    "description"
                    . "The relationship to the disease"
                  )
              )
          )
      )
      (
        "gene as a model of disease association"
          (
            "is_a"
            . "gene to disease association"
          )
          (
            "defining_slots"
              "subject"
              "object"
              "relation"
          )
          (
            "mixins"
              "model to disease mixin"
              "entity to disease association"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "gene or gene product"
                  )
                  (
                    "description"
                    . "A gene that has a role in modeling the disease. This may be a model organism ortholog of a known disease gene, or it may be a gene whose mutants recapitulate core features of the disease."
                  )
              )
          )
      )
      (
        "gene has variant that contributes to disease association"
          (
            "is_a"
            . "gene to disease association"
          )
          (
            "defining_slots"
              "subject"
              "object"
              "relation"
          )
          (
            "slots"
              "sequence variant qualifier"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "gene or gene product"
                  )
                  (
                    "description"
                    . "A gene that has a role in modeling the disease. This may be a model organism ortholog of a known disease gene, or it may be a gene whose mutants recapitulate core features of the disease."
                  )
              )
          )
      )
      (
        "genotype to thing association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
          )
          (
            "abstract"
            . #t
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "genotype"
                  )
                  (
                    "description"
                    . "genotype that is the subject of the association"
                  )
              )
          )
      )
      (
        "gene to expression site association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
              "relation"
          )
          (
            "description"
            . "An association between a gene and an expression site, possibly qualified by stage/timing info."
          )
          (
            "notes"
              "TBD: introduce subclasses for distinction between wild-type and experimental conditions?"
          )
          (
            "see_also"
            . "https://github.com/monarch-initiative/ingest-artifacts/tree/master/sources/BGee"
          )
          (
            "slots"
              "stage qualifier"
              "quantifier qualifier"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "gene or gene product"
                  )
                  (
                    "description"
                    . "gene in which variation is correlated with the phenotypic feature"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "anatomical entity"
                  )
                  (
                    "description"
                    . "location in which the gene is expressed"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "UBERON:0002037"
                        )
                        (
                          "description"
                          . "cerebellum"
                        )
                      )
                  )
              )
              (
                "relation"
                  (
                    "description"
                    . "expression relationship"
                  )
                  (
                    "subproperty_of"
                    . "expressed in"
                  )
              )
              (
                "stage qualifier"
                  (
                    "range"
                    . "life stage"
                  )
                  (
                    "description"
                    . "stage at which the gene is expressed in the site"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "UBERON:0000069"
                        )
                        (
                          "description"
                          . "larval stage"
                        )
                      )
                  )
              )
              (
                "quantifier qualifier"
                  (
                    "description"
                    . "can be used to indicate magnitude, or also ranking"
                  )
              )
          )
      )
      (
        "sequence variant modulates treatment association"
          (
            "is_a"
            . "association"
          )
          (
            "description"
            . "An association between a sequence variant and a treatment or health intervention. The treatment object itself encompasses both the disease and the drug used."
          )
          (
            "comments"
              "An alternate way to model the same information could be via a qualifier"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "abstract"
            . #t
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "sequence variant"
                  )
                  (
                    "description"
                    . "variant that modulates the treatment of some disease"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "treatment"
                  )
                  (
                    "description"
                    . "treatment whose efficacy is modulated by the subject variant"
                  )
              )
          )
      )
      (
        "functional association"
          (
            "is_a"
            . "association"
          )
          (
            "description"
            . "An association between a macromolecular machine (gene, gene product or complex of gene products) and either a molecular activity, a biological process or a cellular location in which a function is executed"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "macromolecular machine"
                  )
                  (
                    "description"
                    . "gene, product or macromolecular complex that has the function associated with the GO term"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "ZFIN:ZDB-GENE-050417-357"
                        )
                        (
                          "description"
                          . "twist1b"
                        )
                      )
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "gene ontology class"
                  )
                  (
                    "description"
                    . "class describing the activity, process or localization of the gene product"
                  )
                  (
                    "values_from"
                      "go"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "GO:0016301"
                        )
                        (
                          "description"
                          . "kinase activity"
                        )
                      )
                      (
                        (
                          "value"
                          . "GO:0045211"
                        )
                        (
                          "description"
                          . "postsynaptic membrane"
                        )
                      )
                  )
              )
          )
      )
      (
        "macromolecular machine to molecular activity association"
          (
            "is_a"
            . "functional association"
          )
          (
            "description"
            . "A functional association between a macromolecular machine (gene, gene product or complex) and a molecular activity (as represented in the GO molecular function branch), where the entity carries out the activity, or contributes to its execution"
          )
          (
            "slot_usage"
              (
                "object"
                  (
                    "range"
                    . "molecular activity"
                  )
              )
          )
      )
      (
        "macromolecular machine to biological process association"
          (
            "is_a"
            . "functional association"
          )
          (
            "description"
            . "A functional association between a macromolecular machine (gene, gene product or complex) and a biological process or pathway (as represented in the GO biological process branch), where the entity carries out some part of the process, regulates it, or acts upstream of it"
          )
          (
            "slot_usage"
              (
                "object"
                  (
                    "range"
                    . "biological process"
                  )
              )
          )
      )
      (
        "macromolecular machine to cellular component association"
          (
            "is_a"
            . "functional association"
          )
          (
            "description"
            . "A functional association between a macromolecular machine (gene, gene product or complex) and a cellular component (as represented in the GO cellular component branch), where the entity carries out its function in the cellular component"
          )
          (
            "slot_usage"
              (
                "object"
                  (
                    "range"
                    . "cellular component"
                  )
              )
          )
      )
      (
        "gene to go term association"
          (
            "aliases"
              "functional association"
          )
          (
            "is_a"
            . "functional association"
          )
          (
            "class_uri"
            . "http://bio2rdf.org/wormbase_vocabulary:Gene-GO-Association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "molecular entity"
                  )
                  (
                    "description"
                    . "gene, product or macromolecular complex that has the function associated with the GO term"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "ZFIN:ZDB-GENE-050417-357"
                        )
                        (
                          "description"
                          . "twist1b"
                        )
                      )
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "gene ontology class"
                  )
                  (
                    "description"
                    . "class describing the activity, process or localization of the gene product"
                  )
                  (
                    "values_from"
                      "go"
                  )
                  (
                    "examples"
                      (
                        (
                          "value"
                          . "GO:0016301"
                        )
                        (
                          "description"
                          . "kinase activity"
                        )
                      )
                  )
              )
          )
      )
      (
        "genomic sequence localization"
          (
            "is_a"
            . "association"
          )
          (
            "description"
            . "A relationship between a sequence feature and an entity it is localized to. The reference entity may be a chromosome, chromosome region or information entity such as a contig"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "aliases"
                      "sequence feature"
                  )
                  (
                    "range"
                    . "genomic entity"
                  )
              )
              (
                "object"
                  (
                    "aliases"
                      "reference"
                  )
                  (
                    "range"
                    . "genomic entity"
                  )
              )
          )
          (
            "slots"
              "start interbase coordinate"
              "end interbase coordinate"
              "genome build"
              "phase"
          )
          (
            "class_uri"
            . "faldo:location"
          )
      )
      (
        "sequence feature relationship"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "description"
            . "For example, a particular exon is part of a particular transcript or gene"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "genomic entity"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "genomic entity"
                  )
              )
          )
      )
      (
        "transcript to gene relationship"
          (
            "is_a"
            . "sequence feature relationship"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "description"
            . "A gene is a collection of transcripts"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "transcript"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "gene"
                  )
              )
          )
      )
      (
        "gene to gene product relationship"
          (
            "is_a"
            . "sequence feature relationship"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "description"
            . "A gene is transcribed and potentially translated to a gene product"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "gene"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "gene product"
                  )
              )
              (
                "relation"
                  (
                    "subproperty_of"
                    . "has gene product"
                  )
              )
          )
      )
      (
        "exon to transcript relationship"
          (
            "is_a"
            . "sequence feature relationship"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "description"
            . "A transcript is formed from multiple exons"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "exon"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "transcript"
                  )
              )
          )
      )
      (
        "gene regulatory relationship"
          (
            "is_a"
            . "association"
          )
          (
            "description"
            . "A regulatory relationship between two genes"
          )
          (
            "slot_usage"
              (
                "relation"
                  (
                    "description"
                    . "the direction is always from regulator to regulated"
                  )
              )
              (
                "subject"
                  (
                    "range"
                    . "gene or gene product"
                  )
                  (
                    "role"
                    . "regulatory gene"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "gene or gene product"
                  )
                  (
                    "role"
                    . "regulated gene"
                  )
              )
          )
      )
      (
        "anatomical entity to anatomical entity association"
          (
            "is_a"
            . "association"
          )
          (
            "defining_slots"
              "subject"
              "object"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "anatomical entity"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "anatomical entity"
                  )
              )
          )
      )
      (
        "anatomical entity to anatomical entity part of association"
          (
            "is_a"
            . "anatomical entity to anatomical entity association"
          )
          (
            "description"
            . "A relationship between two anatomical entities where the relationship is mereological, i.e the two entities are related by parthood. This includes relationships between cellular components and cells, between cells and tissues, tissues and whole organisms"
          )
          (
            "defining_slots"
              "relation"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "anatomical entity"
                  )
                  (
                    "description"
                    . "the part"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "anatomical entity"
                  )
                  (
                    "description"
                    . "the whole"
                  )
              )
              (
                "relation"
                  (
                    "subproperty_of"
                    . "part of"
                  )
              )
          )
      )
      (
        "anatomical entity to anatomical entity ontogenic association"
          (
            "is_a"
            . "anatomical entity to anatomical entity association"
          )
          (
            "description"
            . "A relationship between two anatomical entities where the relationship is ontogenic, i.e the two entities are related by development. A number of different relationship types can be used to specify the precise nature of the relationship"
          )
          (
            "defining_slots"
              "relation"
          )
          (
            "slot_usage"
              (
                "subject"
                  (
                    "range"
                    . "anatomical entity"
                  )
                  (
                    "description"
                    . "the structure at a later time"
                  )
              )
              (
                "object"
                  (
                    "range"
                    . "anatomical entity"
                  )
                  (
                    "description"
                    . "the structure at an earlier time"
                  )
              )
              (
                "relation"
                  (
                    "subproperty_of"
                    . "develops from"
                  )
              )
          )
      )
      (
        "occurrent"
          (
            "description"
            . "A processual entity"
          )
          (
            "is_a"
            . "named thing"
          )
          (
            "class_uri"
            . "BFO:0000003"
          )
      )
      (
        "biological process or activity"
          (
            "is_a"
            . "biological entity"
          )
          (
            "description"
            . "Either an individual molecular activity, or a collection of causally connected molecular activities"
          )
          (
            "id_prefixes"
              "GO"
              "Reactome"
          )
      )
      (
        "molecular activity"
          (
            "is_a"
            . "biological process or activity"
          )
          (
            "aliases"
              "molecular function"
              "molecular event"
              "reaction"
          )
          (
            "mixins"
              "occurrent"
          )
          (
            "description"
            . "An execution of a molecular function carried out by a gene product or macromolecular complex."
          )
          (
            "class_uri"
            . "GO:0003674"
          )
          (
            "mappings"
              "UMLSSC:T044"
              "UMLSST:moft"
          )
          (
            "id_prefixes"
              "GO"
              "Reactome"
          )
      )
      (
        "activity and behavior"
          (
            "is_a"
            . "occurrent"
          )
          (
            "description"
            . "Activity or behavior of any independent integral living, organization or mechanical actor in the world"
          )
          (
            "mappings"
              "UMLSSG:ACTI"
              "UMLSSC:T051"
              "UMLSST:evnt"
              "UMLSSC:T052"
              "UMLSST:acty"
              "UMLSSC:T053"
              "UMLSST:bhvr"
              "UMLSSC:T054"
              "UMLSST:socb"
              "UMLSSC:T055"
              "UMLSST:inbe"
              "UMLSSC:T056"
              "UMLSST:dora"
              "UMLSSC:T057"
              "UMLSST:ocac"
              "UMLSSC:T064"
              "UMLSST:gora"
              "UMLSSC:T066"
              "UMLSST:mcha"
          )
      )
      (
        "procedure"
          (
            "is_a"
            . "occurrent"
          )
          (
            "description"
            . "A series of actions conducted in a certain order or manner"
          )
          (
            "mappings"
              "UMLSSG:PROC"
              "UMLSSC:T058"
              "UMLSST:hlca"
              "UMLSSC:T059"
              "UMLSST:lbpr"
              "UMLSSC:T060"
              "UMLSST:diap"
              "UMLSSC:T061"
              "UMLSST:topp"
              "UMLSSC:T062"
              "UMLSST:resa"
              "UMLSSC:T063"
              "UMLSST:mbrt"
              "UMLSSC:T065"
              "UMLSST:edac"
          )
      )
      (
        "phenomenon"
          (
            "is_a"
            . "occurrent"
          )
          (
            "description"
            . "a fact or situation that is observed to exist or happen, especially one whose cause or explanation is in question"
          )
          (
            "mappings"
              "UMLSSG:PHEN"
              "UMLSSC:T034"
              "UMLSST:lbtr"
              "UMLSSC:T038"
              "UMLSST:biof"
              "UMLSSC:T067"
              "UMLSST:phpr"
              "UMLSSC:T068"
              "UMLSST:hcpp"
              "UMLSSC:T069"
              "UMLSST:eehu"
              "UMLSSC:T070"
              "UMLSST:npop"
          )
      )
      (
        "biological process"
          (
            "is_a"
            . "biological process or activity"
          )
          (
            "mixins"
              "occurrent"
          )
          (
            "description"
            . "One or more causally connected executions of molecular functions"
          )
          (
            "class_uri"
            . "GO:0008150"
          )
          (
            "mappings"
              "SIO:000006"
              "WD:Q2996394"
          )
          (
            "id_prefixes"
              "GO"
              "Reactome"
          )
      )
      (
        "pathway"
          (
            "is_a"
            . "biological process"
          )
          (
            "class_uri"
            . "GO:0007165"
          )
          (
            "mappings"
              "SIO:010526"
              "PW:0000001"
              "WD:Q4915012"
          )
          (
            "id_prefixes"
              "GO"
              "Reactome"
          )
      )
      (
        "physiological process"
          (
            "aliases"
              "physiology"
          )
          (
            "is_a"
            . "biological process"
          )
          (
            "mappings"
              "UMLSSG:PHYS"
              "UMLSSC:T032"
              "UMLSST:orga"
              "UMLSSC:T039"
              "UMLSST:phsf"
              "UMLSSC:T040"
              "UMLSST:orgf"
              "UMLSSC:T041"
              "UMLSST:menp"
              "UMLSSC:T042"
              "UMLSST:ortf"
              "UMLSSC:T043"
              "UMLSST:celf"
              "UMLSSC:T045"
              "UMLSST:genf"
              "UMLSSC:T201"
              "UMLSST:clna"
          )
      )
      (
        "cellular component"
          (
            "is_a"
            . "anatomical entity"
          )
          (
            "description"
            . "A location in or around a cell"
          )
          (
            "class_uri"
            . "GO:0005575"
          )
          (
            "mappings"
              "SIO:001400"
              "WD:Q5058355"
              "UMLSSC:T026"
              "UMLSST:celc"
          )
          (
            "id_prefixes"
              "GO"
          )
      )
      (
        "cell"
          (
            "is_a"
            . "anatomical entity"
          )
          (
            "class_uri"
            . "GO:0005623"
          )
          (
            "mappings"
              "CL:0000000"
              "SIO:010001"
              "WD:Q7868"
              "UMLSSC:T025"
              "UMLSST:cell"
          )
          (
            "id_prefixes"
              "CL"
              "PO"
          )
      )
      (
        "cell line"
          (
            "is_a"
            . "biosample"
          )
          (
            "class_uri"
            . "CLO:0000031"
          )
          (
            "id_prefixes"
              "CLO"
          )
      )
      (
        "gross anatomical structure"
          (
            "aliases"
              "tissue"
              "organ"
          )
          (
            "is_a"
            . "anatomical entity"
          )
          (
            "class_uri"
            . "UBERON:0010000"
          )
          (
            "mappings"
              "SIO:010046"
              "WD:Q4936952"
              "UMLSSC:T017"
              "UMLSST:anst"
              "UMLSSC:T021"
              "UMLSST:ffas"
              "UMLSSC:T023"
              "UMLSST:bpoc"
              "UMLSSC:T024"
              "UMLSST:tisu"
              "UMLSSC:T018"
              "UMLSST:emst"
          )
          (
            "id_prefixes"
              "UBERON"
              "PO"
              "FAO"
          )
      )
  )
)
)
