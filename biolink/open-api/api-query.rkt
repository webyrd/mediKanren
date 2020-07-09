#lang racket
(provide (all-defined-out))
(require json net/url)

;; Broad Institute KP
(define url.broad
  "https://translator.broadinstitute.org/molepro_reasoner/")
(define path.predicates
  "/predicates")
(define path.query
  "/query")

(define (api-query url-string (optional-post-jsexpr (void)))
  (define-values (status headers in)
    (if (void? optional-post-jsexpr)
      (http-sendrecv/url
        (string->url url-string)
        #:method "GET")
      (http-sendrecv/url
        (string->url url-string)
        #:method "POST"
        #:data (jsexpr->string optional-post-jsexpr)
        #:headers '("Content-Type: application/json; charset=utf-8"))))
  (hash 'status status
        'headers headers
        'response (string->jsexpr (port->string in))))

(define (js-query edges nodes)
  (hash 'message
        (hash 'query_graph
              (hash 'edges edges
                    'nodes nodes))))

;; test predicates available on Broad Institute KG
(pretty-print
  (api-query (string-append url.broad path.predicates)))

(pretty-print
  (api-query (string-append url.broad path.query)
             (js-query (list (hash 'id        "e00"
                                   'source_id "n00"
                                   'target_id "n01"
                                   'type      "affects"))
                       (list (hash 'curie "CID:2244"
                                   'id    "n00"
                                   'type  "chemical_substance")
                             (hash 'id    "n01"
                                   'type  "gene")))))
