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
                    "predicate": "biolink:gene_associated_with_condition"
                }
            },
            "nodes": {
                "n00": {
                    "id" : "UniProtKB:P51587",
                    "category": "biolink:biological_entity"
                },
                "n01": {
                    "category": "biolink:Disease"
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
                    "predicate" : "biolink:treats"
                }
            },
            "nodes": {
                "n00": {
                    "id" : "CHEBI:6801XXX"
                },
                "n01": {
                    "category": "biolink:Disease"
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

(define m1    (hash-ref q 'message))
(define m2    (hash-ref q2 'message))

(define r (time (trapi-response m1)))
(display (jsexpr->string r))
(printf "\n=====\n")
(define r2 (time (trapi-response m2)))
(display (jsexpr->string r2))



