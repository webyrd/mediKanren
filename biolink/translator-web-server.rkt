#lang racket/base
(require
  "common.rkt"
  racket/file
  racket/pretty
  racket/runtime-path
  json
  web-server/servlet
  web-server/servlet-env
  web-server/managers/none
  xml
  )

(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define-runtime-path path:root ".")
(define (path/root relative-path) (build-path path:root relative-path))
(define schema.json.txt
  (file->string (path/root "open-api/TranslatorReasonersAPI.json")))
(define schema.yaml.txt
  (file->string (path/root "open-api/TranslatorReasonersAPI.yaml")))
(define schema.html
  (file->string (path/root "open-api/html/index.html")))
(define schema.html2
  (file->string (path/root "open-api/html2/index.html")))
(define schema.json
  (call-with-input-file (path/root "open-api/TranslatorReasonersAPI.json")
                        read-json))
(pretty-print (list 'openapi:      (hash-ref schema.json 'openapi)))
(pretty-print (list 'info:         (hash-ref schema.json 'info)))
(pretty-print (list 'externalDocs: (hash-ref schema.json 'externalDocs)))
(pretty-print (list 'tags:         (hash-ref schema.json 'tags)))
(pretty-print (list 'paths:        (hash-keys (hash-ref schema.json 'paths))))
(pretty-print (list 'components:   (hash-keys (hash-ref schema.json 'components))))

(define (xexpr->html-string xe)
  (string-append "<!doctype html>" (xexpr->string xe)))
(define mime:text (string->bytes/utf-8 "text/plain;charset=utf-8"))
(define mime:html (string->bytes/utf-8 "text/html; charset=utf-8"))
(define mime:json (string->bytes/utf-8 "application/json; charset=utf-8"))
(define (not-found.html uri)
  `(html (head (title "mediKanren: 404"))
         (body (h1 "What are you looking for?")
               (p "There was nothing found at")
               (pre ,uri))))
(define index.html
  `(html (head (title "mediKanren Reasoner API"))
         (body (h1 "mediKanren Reasoner API")
               (p (a ((href "https://github.com/NCATS-Tangerine/NCATS-ReasonerStdAPI"))
                     "NCATS Biomedical Translator Reasoners Standard API"))
               (ul
                 (li (a ((href "/schema.html")) "schema.html"))
                 (li (a ((href "/schema.html2")) "schema.html2"))
                 (li (a ((href "/schema.yaml")) "schema.yaml"))
                 (li (a ((href "/schema.json")) "schema.json")))
               )))

(define (respond code message headers mime-type body)
  (response/full code (string->bytes/utf-8 message)
                 (current-seconds) mime-type headers
                 (list (string->bytes/utf-8 body))))

(define (not-found req)
  (respond 404 "Not Found" '() mime:html
           (xexpr->html-string (not-found.html
                                 (url->string (request-uri req))))))
(define (index req)
  (respond 200 "ok" '() mime:html (xexpr->html-string index.html)))
(define (schema:json  req) (respond 200 "ok" '() mime:text schema.json.txt))
(define (schema:yaml  req) (respond 200 "ok" '() mime:text schema.yaml.txt))
(define (schema:html  req) (respond 200 "ok" '() mime:html schema.html))
(define (schema:html2 req) (respond 200 "ok" '() mime:html schema.html2))

(define (start)
  (define-values (dispatch _)
    (dispatch-rules
      (("")             #:method         "get" index)
      (("schema.json")  #:method         "get" schema:json)
      (("schema.yaml")  #:method         "get" schema:yaml)
      (("schema.html")  #:method         "get" schema:html)
      (("schema.html2") #:method         "get" schema:html2)
      (else                                    not-found)))
  ;; after loading this file, launch the server by evaluating
  (serve/servlet dispatch
                 ;; none-manager for better performance:
                 ;; only possible because we're not using web continuations.
                 #:manager (create-none-manager #f)
                 #:servlet-regexp #rx""
                 #:launch-browser? #f))

(module+ main (start))
