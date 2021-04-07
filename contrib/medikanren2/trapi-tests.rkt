#lang racket/base
(provide trapi-response)
(require
  "../../medikanren2/trapi.rkt"
  racket/file racket/function racket/list racket/hash
  (except-in racket/match ==)
  racket/port
  racket/pretty
  racket/runtime-path
  racket/string
  json
  )

;; Simple tests for TRAPI interpreter

(define q (string->jsexpr #<<EOS
{
    "message": {
        "query_graph": {
            "edges": {
                "e00": {
                    "subject": "n00",
                    "object": "n01",
                    "predicates": ["biolink:gene_associated_with_condition"]
                }
            },
            "nodes": {
                "n00": {
                    "ids" : ["UniProtKB:P51587"],
                    "categories": ["biolink:biological_entity"]
                },
                "n01": {
                    "categories": ["biolink:Disease"]
                }
            }
        }
    }
}
EOS
))

;; Using knowledge_graph

(define q2 (string->jsexpr #<<EOS
{
    "message": {
        "query_graph": {
            "edges": {
                "e00": {
                    "subject": "n00",
                    "object": "n01",
                    "predicates" : ["biolink:treats"]
                }
            },
            "nodes": {
                "n00": {
                    "ids" : ["CHEBI:6801XXX"]
                },
                "n01": {
                    "categories": ["biolink:Disease"]
                }
            }
        },
       "knowledge_graph" : {
           "nodes": {
               "MONDO:0005148": {"name": "type-2 diabetes", "category":"biolink:Disease"},
               "CHEBI:6801XXX": {"name": "metformin", "category": "drug"}
            },
           "edges": {
              "df87ff82": {"subject": "CHEBI:6801XXX", "predicate": "biolink:treats", "object": "MONDO:0005148"}
            }
         }
    }
}
EOS
))

(define q3 (string->jsexpr #<<EOS
{
  "message": {
    "query_graph": {
      "edges": {
        "e01": {
          "object": "n0",
          "subject": "n1",
          "predicates":["biolink:has_phenotype"]
        }
      },
      "nodes": {
        "n0": {
          "ids": ["MONDO:0007114"],
          "categories":["biolink:Disease"]
        },
        "n1": {
          "categories": ["biolink:PhenotypicFeature"]
        }
      }
    }
  }
 }
EOS
))

(define m1    (hash-ref q 'message))
(define m2    (hash-ref q2 'message))
(define m3 (hash-ref q3 'message))

(define r (time (trapi-response m1)))
(display (jsexpr->string r))
(printf "\n=====\n")
(define r2 (time (trapi-response m2)))
(display (jsexpr->string r2))

(define r3 (time (trapi-response m3)))

