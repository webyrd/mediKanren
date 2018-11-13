#lang racket
(require "workflow1.rkt"
         web-server/servlet
         web-server/servlet-env
         web-server/managers/none
         net/uri-codec
         xml
         json)

(define index.html
  `(html (head (title "mediKanren for workflows"))
         (body (p "POST your queries in JSON format to /query")
               (p (a ((href "https://github.com/NCATS-Tangerine/NCATS-ReasonerStdAPI"))
                     "Standard"))
               (p (a ((href "https://www.dropbox.com/s/auqt6gedhnz5bbt/WF1MOD1_results_DOID9352.json?dl=0"))
                     "Example input"))
               (p (a ((href "https://www.dropbox.com/s/u6zik652qojkmgb/WF1MOD1-MOD2_results_DOID9352.json?dl=0"))
                     "Example output")))))

(define (not-found.html uri)
  `(html (head (title "Sad Face"))
         (body (h1 "What are you looking for?")
               (p "There was nothing found at")
               (pre ,uri))))

(define (omim? cui) (string-prefix? cui "OMIM:"))

(define (json->cuis jsdata)
  (map (lambda (r)
         (define essence (hash-ref r 'essence))
         (define (essence? n) (equal? (hash-ref n 'name) essence))
         (define nodes (hash-ref (hash-ref r 'result_graph) 'node_list))
         (car (filter omim? (map (lambda (n) (hash-ref n 'id))
                                 (filter essence? nodes)))))
       (hash-ref jsdata 'result_list)))

(define reasoner-id "mediKanren-0.00000000000000zerozero...")

(define (concept->json concept)
  (define cui (list-ref concept 2))
  (define name (list-ref concept 3))
  (define props (list-tail concept 4))
  (define description (cdr (or (assoc "description" props) '(#f . ""))))
  (define uri         (cdr (or (assoc "uri"         props) '(#f . ""))))
  (define node
    (hash 'description     description
          'id              cui
          'name            name
          'node_attributes '()
          'type            "chemical_substance"
          'uri             uri))
  (hash 'confidence  0.00000000000000001
        'description ""
        'essence     name
        'id          ""
        'reasoner_id reasoner-id
        'result_graph (hash 'edge_list '()
                            'node_list (list node))
        'result_type                   ""
        'row_data                      '()
        'text                          (string-append "Something about: " name)))

(define (concepts->json concepts)
  (hash 'context                "https://raw.githubusercontent.com/biolink/biolink-model/master/context.jsonld"
        'datetime               "2018-09-20 20:45:05"
        'id                     ""
        'message                ""
        'n_results              1
        'original_question_text ""
        'query_type_id          ""
        'reasoner_id            reasoner-id
        'response_code          "OK"
        'restated_question_text ""
        'result_list            (map concept->json concepts)
        'schema_version         "0.8.0"
        'table_column_names     '()
        'terms                  (hash 'disease "")
        'tool_version           "Nope"
        'type                   "medical_translator_query_result"))

(define (query.json jsdata)
  (define cuis (json->cuis jsdata))
  (displayln `(cuis-received: ,cuis))
  (define concepts (append* (map workflow1module2 cuis)))
  (concepts->json concepts))


(define mime:html (string->bytes/utf-8 "text/html; charset=utf-8"))
(define mime:json (string->bytes/utf-8 "application/json; charset=utf-8"))


(define (respond code message headers mime-type body)
  (response/full code (string->bytes/utf-8 message)
                 (current-seconds) mime-type headers
                 (list (string->bytes/utf-8 body))))

(define (index req)
  (respond 200 "ok" '() mime:html (xexpr->html-string index.html)))

(define (query req)
  (respond 200 "OK" '() mime:json
           (jsexpr->string
             (query.json
               (bytes->jsexpr (request-post-data/raw req))))))

(define (not-found req)
  (respond 404 "Not Found" '() mime:html
           (xexpr->html-string (not-found.html
                                 (url->string (request-uri req))))))

;(define (repr d) (with-output-to-string (lambda () (write d))))
(define (xexpr->html-string xe)
  (string-append "<!doctype html>" (xexpr->string xe)))


(define-values (dispatcher _)
  (dispatch-rules
    (("")      #:method         "get" index)
    (("query") #:method (regexp ".*") query)
    (else                             not-found)))

;; after loading this file, launch the server by evaluating
(serve/servlet dispatcher
               #:manager (create-none-manager #f)  ;; better performance if you're not using web continuations
               #:servlet-regexp #rx""
               #:launch-browser? #f)
