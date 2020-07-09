#lang racket
(provide (all-defined-out))
(require json net/url)

(define (js-count js)
  (cond ((pair? js) (foldl + 0 (map js-count js)))
        ((hash? js) (foldl + 0 (hash-map js (lambda (k v) (js-count v)))))
        (else       1)))

;; Broad Institute KP
(define url.broad
  "https://translator.broadinstitute.org/molepro_reasoner")
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

(module+ main
  ;; test predicates available on Broad Institute KG
  ;(pretty-print
    ;(api-query (string-append url.broad path.predicates)))

  ;(pretty-print
    ;(api-query (string-append url.broad path.query)
               ;(js-query (list (hash 'id        "e00"
                                     ;'source_id "n00"
                                     ;'target_id "n01"
                                     ;'type      "affects"))
                         ;(list (hash 'curie "CID:2244"
                                     ;'id    "n00"
                                     ;'type  "chemical_substance")
                               ;(hash 'id    "n01"
                                     ;'type  "gene")))))

  ;(js-count
    ;(time (api-query (string-append url.unsecret path.predicates))))

  ;(pretty-print
    ;(time
      ;(api-query (string-append url.unsecret path.query)
                 ;(js-query (list (hash 'id        "e00"
                                       ;'source_id "n00"
                                       ;'target_id "n01"
                                       ;'type      "causes"))
                           ;(list (hash 'curie "UMLS:C0004096"
                                       ;'id    "n01"
                                       ;'type  "disease")
                                 ;(hash 'id    "n00"
                                       ;'type  "gene"))))))

  ;(pretty-print
    ;(time
      ;(api-query (string-append url.broad path.query)
                 ;(js-query (list (hash 'id        "e00"
                                       ;'source_id "n00"
                                       ;'target_id "n01"
                                       ;'type      "causes"))
                           ;(list (hash 'curie "UMLS:C0004096"
                                       ;'id    "n01"
                                       ;'type  "disease")
                                 ;(hash 'id    "n00"
                                       ;'type  "gene"))))))

  (pretty-print
    (time (api-query (string-append url.unsecret path.query)
                     (js-query (list (hash 'id        "e00"
                                           'source_id "n00"
                                           'target_id "n01"
                                           'type      "affects"))
                               (list (hash 'curie "CID:2244"
                                           'id    "n00"
                                           'type  "chemical_substance")
                                     (hash 'id    "n01"
                                           'type  "gene"))))))
)
