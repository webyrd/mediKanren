#lang racket
(provide (all-defined-out))
(require net/url)
(require json)

;; Broad Institute KP
(define url
  "https://translator.broadinstitute.org/molepro_reasoner/") 
(define predicates
  "/predicates")

(define test
  (string-append url predicates))

(define api-query
  (lambda (api-url)
    (call/input-url
     (string->url api-url)
     get-pure-port
     (lambda (port)
       (string->jsexpr (port->string port))))))

;; test predicates available on Broad Institute KG
(api-query test)


(define query-hash
  (hash 'message (list (hash 'query_graph "query_graph" 'id "e00" 'source_id "n00" 'target_id "n01" 'type "affects" ))))  

(define test-query (string-append url (jsexpr->string query-hash)))




