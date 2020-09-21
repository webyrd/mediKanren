#lang racket
(provide (all-defined-out))
(require json net/url)

(define (js-count js)
  (cond ((pair? js) (foldl + 0 (map js-count js)))
        ((hash? js) (foldl + 0 (hash-map js (lambda (k v) (js-count v)))))
        (else       1)))

;; drug response kg
(define url.multiomics/drug-response
  "https://biothings.ncats.io/drug_response_kp")

;; cancer gene mutation frequnency 
(define url.multiomics/tcga-mut-freq
  "https://biothings.ncats.io/tcga_mut_freq_kp")

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

;; query with gene and get back mutation prevalence by cancer-type
(define tcga_query:Gene--has-mutation-prevalence-in-cancer-type->DiseaseX/pvalue=.05
  (lambda (HGNC-gene-symbol)
    (string-append
     "/query?q=association.freq_by_case:%3E0.05%20AND%20subject.SYMBOL:"
     (symbol->string HGNC-gene-symbol))))

(pretty-print
 (time
  (api-query
   (string-append
    url.multiomics/tcga-mut-freq
    (tcga_query:Gene--has-mutation-prevalence-in-cancer-type->DiseaseX/pvalue=.05 'EGFR)))))



(define tcga_query:GeneX--has-mutation-prevalence-in-cancer-type->Disease/pvalue=.05
  (lambda (mondo-curie)
    (string-append
     "/query?q=object.id:%22"
     (string-append mondo-curie "%22%20AND%20association.freq_by_case:%3E0.03"))))

;; disease --> gene w/ frequency of mutation 
;; "MONDO:0006256" = invasive breast carcinoma
(pretty-print
 (time
  (api-query
   "https://biothings.ncats.io/tcga_mut_freq_kp/query?q=object.id:%22MONDO:0006256%22%20AND%20association.freq_by_case:%3E0.03&size=1000"
   #;(string-append
    url.multiomics/tcga-mut-freq
    (tcga_query:GeneX--has-mutation-prevalence-in-cancer-type->Disease/pvalue=.05 "MONDO:0007254"))
   )))

