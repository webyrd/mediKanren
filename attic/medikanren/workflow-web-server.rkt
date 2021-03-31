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

(define (json->all-cuis jsdata)
  (map (lambda (named-cui)
         ;(define name (cadr jsdata))
         (car named-cui))
       jsdata))

(define reasoner-id "mediKanren-0.00000000000000zerozero...")

(define rem-dups
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(member (car ls) (cdr ls)) (rem-dups (cdr ls))]
      [else (cons (car ls) (rem-dups (cdr ls)))])))

(define (edge->json edge)
  (define db (car edge))
  (define type (cdr (cadddr (cdr edge))))
  (define src (cadr (caddr edge)))
  (define tgt (cadr (cadddr edge)))
  (define eprops (cddddr (cdr edge)))
  (define kpmids (assoc "pmids" eprops))
  (define kpubs (assoc "publications" eprops))
  (define pmids (rem-dups (if kpmids (cdr kpmids) '())))
  (define pubs (rem-dups (if kpubs (cdr kpubs) '())))
  (define publications
    (append (map (lambda (id)
                   (string-append "https://www.ncbi.nlm.nih.gov/pubmed/" id))
                 pmids) pubs))
  (hash 'type           type
        'source_id      src
        'target_id      tgt
        'provided_by    db
        'confidence     0.00000000000000001
        'publications   publications
        'attribute_list '()))

(define (concept->json concept)
  (define cui (list-ref concept 2))
  (define name (list-ref concept 3))
  ;; TODO: more general type.
  (define type "chemical_substance")
  (define props (list-tail concept 4))
  (define description (cdr (or (assoc "description" props) '(#f . ""))))
  (define uri         (cdr (or (assoc "uri"         props) '(#f . ""))))
  (hash 'description     description
        'id              cui
        'name            name
        'type            type
        'uri             uri
        'node_attributes '()))

(define (result->json result)
  ;; TODO:
  (define essence    "")
  (define confidence 0.00000000000000001)
  (define concepts   (cdr (assoc 'concepts result)))
  (define edges      (cdr (assoc 'edges result)))
  (hash 'confidence   confidence
        'description  ""
        'essence      essence
        'id           ""
        'reasoner_id  reasoner-id
        'result_graph (hash 'edge_list (map edge->json    edges)
                            'node_list (map concept->json concepts))
        'result_type  "answer"
        'row_data     '()
        'text         ""))

(define (response->json results)
  (define (pad n) (~a n #:width 2 #:align 'right #:pad-string "0"))
  (define datetime
    (let ((dt (seconds->date (current-seconds) #f)))
      (format
        "~a-~a-~a ~a:~a:~a"
        (date-year dt) (pad (date-month dt)) (pad (date-day dt))
        (pad (date-hour dt)) (pad (date-minute dt)) (pad (date-second dt)))))
  (hash 'context                "https://raw.githubusercontent.com/biolink/biolink-model/master/context.jsonld"
        'datetime               datetime
        'id                     ""
        'message                ""
        'n_results              (length results)
        'original_question_text ""
        'query_type_id          ""
        'reasoner_id            reasoner-id
        'response_code          "OK"
        'restated_question_text ""
        'result_list            (map result->json results)
        'schema_version         "0.8.0"
        'table_column_names     '()
        'terms                  (hash 'disease "")
        'tool_version           "Nope"
        'type                   "medical_translator_query_result"))

(define (query.json jsdata)
  (define cuis (json->cuis jsdata))
  (displayln `(cuis-received: ,cuis))
  (define concepts (append* (map workflow1module2 cuis)))
  (response->json (list (list (cons 'concepts concepts) (cons 'edges '())))))

(define (direct-query.json jsdata)
  (define cuis (json->all-cuis jsdata))
  (displayln `(cuis-received: ,cuis))
  (define concepts (append* (map workflow1module2 cuis)))
  (response->json (list (list (cons 'concepts concepts) (cons 'edges '())))) )

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

(define (direct-query req)
  (respond 200 "OK" '() mime:json
           (jsexpr->string
             (direct-query.json
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
    (("")             #:method         "get" index)
    (("query")        #:method (regexp ".*") query)
    (("direct-query") #:method (regexp ".*") direct-query)
    (else                                    not-found)))

;; after loading this file, launch the server by evaluating
(serve/servlet dispatcher
               #:manager (create-none-manager #f)  ;; better performance if you're not using web continuations
               #:servlet-regexp #rx""
               #:launch-browser? #f)
