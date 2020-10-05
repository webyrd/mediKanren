#lang racket
(provide (all-defined-out))
(require json net/url
         nested-hash)

(define (js-count js)
  (cond ((pair? js) (foldl + 0 (map js-count js)))
        ((hash? js) (foldl + 0 (hash-map js (lambda (k v) (js-count v)))))
        (else       1)))

;; Broad Institute KP GENETICS PROVIDERS
;; DATASETS: NOT SHOWN
;; GENE (NCBI) <--> DISEASE (EFO)
;; DISEASE --> PATHWAY . DISEASE AS 0 EDGE

(define url.broad
  "https://translator.broadinstitute.org/genetics_data_provider")

;; Broad Institute KP specific for CHEMBL queries
(define url.broad_chembl
  "https://translator.broadinstitute.org/chembl")

(define chembl-drug-indications/transform
  "/indications/transform")

(define url.unsecret
  "https://unsecret.ncats.io")
(define path.predicates
  "/predicates")
(define path.query
  "/query")

(define (api-query url-string (optional-post-jsexpr (void)))
  (define-values (status headers in)
    (time (if (void? optional-post-jsexpr)
            (http-sendrecv/url
              (string->url url-string)
              #:method "GET")
            (http-sendrecv/url
              (string->url url-string)
              #:method "POST"
              #:data (jsexpr->string optional-post-jsexpr)
              #:headers '("Content-Type: application/json; charset=utf-8")))))
  (define response-string (time (port->string in)))
  (displayln (string-length response-string))
  (pretty-print headers)
  (hash 'status status
        'headers headers
        'response (string->jsexpr response-string)))

(define (js-query edges nodes)
  (hash 'message
        (hash 'query_graph
              (hash 'edges edges
                    'nodes nodes))))


;; X genes --associated--> Asthma EFO:0000270
(pretty-print
 (time (api-query (string-append url.broad path.query)
                  (js-query (list (hash 'id        "e00"
                                        'source_id "n00"
                                        'target_id "n01"
                                        'type      "associated"))
                            (list (hash 'curie "EFO:0000270"
                                        'id    "n00"
                                        'type  "disease")
                                  (hash 'id    "n01"
                                        'type  "gene"))))))

(define geneticsprovider:disease->gene
  (time (api-query (string-append url.broad path.query)
                  (js-query (list (hash 'id        "e00"
                                        'source_id "n00"
                                        'target_id "n01"
                                        'type      "associated"))
                            (list (hash 'curie "EFO:0000270"
                                        'id    "n00"
                                        'type  "disease")
                                  (hash 'id    "n01"
                                        'type  "gene"))))))

#|
(define edges-from-geneticsprovider:disease->gene
  (let* ((edges (nested-hash-ref* geneticsprovider:disease->gene '(response knowledge_graph edges)))
        (subject (map (lambda (hasheq) (hash-ref hasheq 'source_id)) edges))
        (predicate (map (lambda (hasheq) (hash-ref hasheq 'type)) edges))
        (object (map (lambda (hasheq) (hash-ref hasheq 'target_id)) edges))
        (evidence (map (lambda (hasheq) (hash-ref hasheq 'score)) edges))
        (provinance (map (lambda (hasheq) (hash-ref hasheq 'score_name)) edges)))
(list subject predicate object evidence provinance)))
|#



#|
{
  "message": {
    "query_graph": {
      "edges": [
        {
          "id": "e00",
          "source_id": "n00",
          "target_id": "n01",
          "type": "associated"
        }
      ],
      "nodes": [
        {
          "curie": "EFO:0000270",
          "id": "n00",
          "type": "disease"
        },
        {
          "id": "n01",
          "type": "gene"
        }
      ]
    }
  }
}

|#
