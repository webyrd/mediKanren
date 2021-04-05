(define example1
'(
  (
    "id"
    . "http://example.org/sample/example1"
  )
  (
    "name"
    . "synopsis2"
  )
  (
    "prefixes"
      (
        "foaf"
        . "http://xmlns.com/foaf/0.1/"
      )
      (
        "samp"
        . "http://example.org/model/"
      )
      (
        "xsd"
        . "http://www.w3.org/2001/XMLSchema#"
      )
  )
  (
    "default_prefix"
    . "samp"
  )
  (
    "default_curi_maps"
      "semweb_context"
  )
  (
    "default_range"
    . "string"
  )
  (
    "types"
      (
        "string"
          (
            "base"
            . "str"
          )
          (
            "uri"
            . "xsd:string"
          )
      )
      (
        "int"
          (
            "base"
            . "int"
          )
          (
            "uri"
            . "xsd:integer"
          )
      )
      (
        "boolean"
          (
            "base"
            . "Bool"
          )
          (
            "uri"
            . "xsd:boolean"
          )
      )
  )
  (
    "classes"
      (
        "person"
          (
            "description"
            . "A person, living or dead"
          )
          (
            "slots"
              "id"
              "first name"
              "last name"
              "age"
              "living"
              "knows"
          )
      )
      (
        "friendly_person"
          (
            "description"
            . "Any person that knows someone"
          )
          (
            "is_a"
            . "person"
          )
          (
            "slot_usage"
              (
                "knows"
                  (
                    "required"
                    . #t
                  )
              )
          )
      )
  )
  (
    "slots"
      (
        "id"
          (
            "description"
            . "Unique identifier of a person"
          )
          (
            "identifier"
            . #t
          )
      )
      (
        "first name"
          (
            "description"
            . "The first name of a person"
          )
          (
            "slot_uri"
            . "foaf:firstName"
          )
          (
            "multivalued"
            . #t
          )
      )
      (
        "last name"
          (
            "description"
            . "The last name of a person"
          )
          (
            "slot_uri"
            . "foaf:lastName"
          )
          (
            "required"
            . #t
          )
      )
      (
        "living"
          (
            "description"
            . "Whether the person is alive"
          )
          (
            "range"
            . "boolean"
          )
          (
            "comments"
              "unspecified means unknown"
          )
      )
      (
        "age"
          (
            "description"
            . "The age of a person if living or age of death if not"
          )
          (
            "range"
            . "int"
          )
          (
            "slot_uri"
            . "foaf:age"
          )
      )
      (
        "knows"
          (
            "description"
            . "A person known by this person (indicating some level of reciprocated interaction between the parties)."
          )
          (
            "range"
            . "person"
          )
          (
            "slot_uri"
            . "foaf:knows"
          )
          (
            "multivalued"
            . #t
          )
      )
  )
)
)
