#lang racket/base
(require
  "common.rkt"
  racket/file
  racket/list
  (except-in racket/match ==)
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
(define argv (current-command-line-arguments))
(define argv-optional '#(CONFIG_FILE))
(when (not (<= (vector-length argv) (vector-length argv-optional)))
  (error "optional arguments ~s; given ~s" argv-optional argv))
;; Loading will occur at first use if not explicitly forced like this.
(load-config #t (and (<= 1 (vector-length argv)) (vector-ref argv 0)))
(load-databases #t)

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
(define mime:js   (string->bytes/utf-8 "text/javascript;charset=utf-8"))
(define mime:json (string->bytes/utf-8 "application/json; charset=utf-8"))
(define index.js "
window.addEventListener('load', function(){
var query_result       = document.getElementById('query-result');
var query_result_clear = document.getElementById('query-result-clear');
var query_form         = document.getElementById('query-form');
var query_text         = document.getElementById('query-text');
var query_submit       = document.getElementById('query-submit');
function pretty_json(json_text) {
  try { return JSON.stringify(JSON.parse(json_text), null, 2); }
  catch (_) { return json_text; }
}
function query(data) {
  var xhr = new XMLHttpRequest();
  xhr.addEventListener('load',  function(event){ show(xhr.responseText); });
  xhr.addEventListener('error', function(event){ show('POST error'); });
  xhr.open('POST', '/query');
  xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
  xhr.send(data);
}
function show(result)   { query_result.textContent = pretty_json(result); }
function clear_result() { query_result.textContent = ''; }

query_text.textContent = pretty_json(query_text.textContent);
query_form.addEventListener('submit', function(event){
  event.preventDefault();
  query(query_text.value);
}, false);
query_result_clear.addEventListener('click', function(){
  clear_result();
}, false);
});")
(define (not-found.html uri)
  `(html (head (title "mediKanren: 404"))
         (body (h1 "What are you looking for?")
               (p "There was nothing found at")
               (pre ,uri))))
(define index.html
  `(html (head (title "mediKanren Reasoner API")
               (script ((src "/index.js"))))
         (body (h1 "mediKanren Reasoner API")
               (p (a ((href "https://github.com/NCATS-Tangerine/NCATS-ReasonerStdAPI"))
                     "NCATS Biomedical Translator Reasoners Standard API"))
               (ul (li (a ((href "/schema.html")) "schema.html"))
                   (li (a ((href "/schema.html2")) "schema.html2"))
                   (li (a ((href "/schema.yaml")) "schema.yaml"))
                   (li (a ((href "/schema.json")) "schema.json")))
               (p (a ((href "/predicates")) "GET /predicates"))
               (form ((method "post") (action "/query") (id "query-form"))
                     (div (textarea
                            ((id "query-text"))
                            "{
  \"message\": {
    \"query_graph\": {
      \"nodes\": [
        {
          \"id\": \"n0\",
          \"curie\": \"UMLS:C0935989\"
        },
        {
          \"id\": \"n1\",
          \"type\": \"gene\"
        },
        {
          \"id\": \"n2\",
          \"curie\": \"UMLS:C0004096\"
        }
      ],
      \"edges\": [
        {
          \"id\": \"e0\",
          \"type\": \"negatively_regulates\",
          \"source_id\": \"n0\",
          \"target_id\": \"n1\"
        },
        {
          \"id\": \"e1\",
          \"type\": \"gene_associated_with_condition\",
          \"source_id\": \"n1\",
          \"target_id\": \"n2\"
        }
      ]
    }
  }
}"
                            ;,(jsexpr->string
                               ;;; TODO: factor this out
                               ;(hash 'message
                                     ;(hash 'query_graph
                                           ;(hash 'nodes (list (hash 'id ""
                                                                    ;'curie ""
                                                                    ;'type ""))
                                                 ;'edges (list (hash 'id ""
                                                                    ;'type ""
                                                                    ;'source_id ""
                                                                    ;'target_id ""))))))
                            ))
                     (div (button ((type "submit") (id "query-submit"))
                                  "POST /query")))
               (div (button ((id "query-result-clear")) "Clear Result"))
               (div (pre ((id "query-result")) "Result will appear here.")))))

(define hash-empty (hash))
(define (olift v) (if (hash? v) v hash-empty))
(define (slift v) (cond ((pair? v) v) ((string? v) (list v)) (else '())))
(define (str v)   (and (string? v) v))

(define (concept->result c)
  (hash 'id (caddr c) 'name (cadddr c) 'type (cdr (cadddr (cdr c)))))
(define (edge->result e)
  (define id (string-append (symbol->string (car e)) (number->string (cadr e))))
  (define src (cadr (caddr e)))
  (define tgt (cadr (cadddr e)))
  (define pred (cdr (cadddr (cdr e))))
  (define attrs (make-immutable-hash
                  (map (lambda (kv) (cons (string->symbol (car kv)) (cdr kv)))
                       (cadddr (cddr e)))))
  (hash 'id id 'type pred 'source_id src 'target_id tgt 'attrs attrs))

(define (message->response msg)
  ;; NOTE: ignore 'results and 'knowledge_graph until we find a use for them.
  (define qgraph (hash-ref msg 'query_graph hash-empty))
  (define nodes
    (filter-not not
      (map (lambda (n)
             (let ((id    (str   (hash-ref n 'id    #f)))
                   (curie (slift (hash-ref n 'curie '())))
                   (type  (slift (hash-ref n 'type  '()))))
               ;; TODO: use a new find-concepts based on xrefo instead?
               (and id `(,(string->symbol id)
                          ',(cond ((pair? curie) (find-concepts #t curie))
                                  ((pair? type)  (find-categories type))
                                  (else          #f))))))
           (hash-ref qgraph 'nodes '()))))
  (define edges&paths
    (filter-not not
      (map (lambda (e)
             (let ((id   (str   (hash-ref e 'id        #f)))
                   (type (slift (hash-ref e 'type      '())))
                   (src  (str   (hash-ref e 'source_id #f)))
                   (tgt  (str   (hash-ref e 'target_id #f))))
               (define preds (and (pair? type) (find-predicates type)))
               (and id src tgt (cons `(,(string->symbol id) ',preds)
                                     (map string->symbol (list src id tgt))))))
           (hash-ref qgraph 'edges '()))))
  (define edges (map car edges&paths))
  (define paths (map cdr edges&paths))
  (define runq #`(run/graph #,nodes #,edges . #,paths))
  (displayln "===============================================================")
  (pretty-print (syntax->datum runq))
  (match-define (list name=>concepts name=>edges) (time (eval runq)))
  (displayln "result counts:")
  (pretty-print (hash-map name=>concepts (lambda (n xs) (cons n (length xs)))))
  (pretty-print (hash-map name=>edges    (lambda (n xs) (cons n (length xs)))))
  (define knodes
    (hash-map name=>concepts (lambda (name xs) (map concept->result xs))))
  (define kedges
    (hash-map name=>edges    (lambda (name xs) (map edge->result xs))))
  (hash 'results
        (append* (map (lambda (p)
                        (define qsrc  (symbol->string (car p)))
                        (define qname (cadr p))
                        (define qedge (symbol->string qname))
                        (define qtgt  (symbol->string (caddr p)))
                        (map (lambda (e)
                               (define r (edge->result e))
                               (define id  (hash-ref r 'id))
                               (define src (hash-ref r 'source_id))
                               (define tgt (hash-ref r 'target_id))
                               (hash 'edge_bindings
                                     (list (hash 'qg_id qedge 'kg_id id))
                                     'node_bindings
                                     (list (hash 'qg_id qsrc  'kg_id src)
                                           (hash 'qg_id qtgt  'kg_id tgt))))
                             (hash-ref name=>edges qname)))
                      paths))
        'knowledge_graph (hash 'nodes (append* knodes)
                               'edges (append* kedges))))

(define (predicates)
  ;; TODO: at greater expense, we could restrict each list of predicates
  ;; to those reflected by existing edges for the corresponding categories.
  (define cs (run* (c) (categoryo c)))
  (define ps (run* (p) (predicateo p)))
  (define dbs (map car cs))
  (make-immutable-hash
    (append*
      (map (lambda (db)
             (define dcs
               (filter-not
                 not (map (lambda (c) (and (eq? (car c) db) (cddr c)))
                          cs)))
             (define dps
               (filter-not
                 not (map (lambda (p) (and (eq? (car p) db) (cddr p)))
                          ps)))
             (define dcps
               (make-immutable-hash
                 (map (lambda (c) (cons (string->symbol c) dps)) dcs)))
             (map (lambda (c) (cons (string->symbol c) dcps)) dcs))
           dbs))))
(define (query jsdata)
  (cond ((or (eof-object? jsdata) (not (hash? jsdata))) 'null)
        (else (message->response
                (olift (hash-ref (olift jsdata) 'message hash-empty))))))

(define (respond code message headers mime-type body)
  (response/full code (string->bytes/utf-8 message)
                 (current-seconds) mime-type headers
                 (list (string->bytes/utf-8 body))))
(define (OK mime-type body) (respond 200 "OK" '() mime-type body))
(define (not-found req)
  (respond 404 "Not Found" '() mime:html
           (xexpr->html-string (not-found.html
                                 (url->string (request-uri req))))))
(define (index         req) (OK mime:html (xexpr->html-string index.html)))
(define (/index.js     req) (OK mime:js   index.js))
(define (/schema.json  req) (OK mime:text schema.json.txt))
(define (/schema.yaml  req) (OK mime:text schema.yaml.txt))
(define (/schema.html  req) (OK mime:html schema.html))
(define (/schema.html2 req) (OK mime:html schema.html2))
(define (/predicates   req) (OK mime:json (jsexpr->string (predicates))))
(define (/query        req)
  (OK mime:json
      (jsexpr->string (query (bytes->jsexpr (request-post-data/raw req))))))

(define (start)
  (define-values (dispatch _)
    (dispatch-rules
      (("")             #:method         "get"  index)
      (("index.js")     #:method         "get"  /index.js)
      (("schema.json")  #:method         "get"  /schema.json)
      (("schema.yaml")  #:method         "get"  /schema.yaml)
      (("schema.html")  #:method         "get"  /schema.html)
      (("schema.html2") #:method         "get"  /schema.html2)
      (("predicates")   #:method         "get"  /predicates)
      (("query")        #:method         "post" /query)
      (else                                     not-found)))
  (serve/servlet dispatch
                 ;; none-manager for better performance:
                 ;; only possible because we're not using web continuations.
                 #:manager (create-none-manager #f)
                 #:servlet-regexp #rx""
                 #:launch-browser? #f))

(module+ main (start))
