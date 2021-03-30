#lang racket/base
(require
  "common.rkt"
  "trapi.rkt"
  "open-api/api-query.rkt"
  ;; "open-api/api-query.rkt"
  ;; "pieces-parts/synonymize.rkt"
  racket/file racket/function racket/list racket/hash
  (except-in racket/match ==)
  racket/port
  racket/pretty
  racket/runtime-path
  racket/string
  json
  web-server/servlet
  web-server/servlet-env
  web-server/managers/none
  web-server/private/gzip
  xml
  )

(define (alist-ref alist key default)
  (define kv (assoc key alist))
  (if kv (cdr kv) default))

(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)
(define argv (current-command-line-arguments))
(define argv-optional '#(CONFIG_FILE))
(when (not (<= (vector-length argv) (vector-length argv-optional)))
  (error "optional arguments ~s; given ~s" argv-optional argv))
;; Loading will occur at first use if not explicitly forced like this.
(load-config #t (and (<= 1 (vector-length argv)) (vector-ref argv 0)))
;; (load-databases #t)

(define-runtime-path path:root ".")
(define (path/root relative-path) (build-path path:root relative-path))

;; (define schema.json.txt
;;   (file->string (path/root "open-api/TranslatorReasonersAPI.json")))
;; (define schema.yaml.txt
;;   (file->string (path/root "open-api/TranslatorReasonersAPI.yaml")))
;; (define schema.html
;;   (file->string (path/root "open-api/html/index.html")))
;; (define schema.html2
;;   (file->string (path/root "open-api/html2/index.html")))
;; (define schema.json
;;   (call-with-input-file (path/root "open-api/TranslatorReasonersAPI.json")
;;                         read-json))

;; (pretty-print (list 'openapi:      (hash-ref schema.json 'openapi)))
;; (pretty-print (list 'info:         (hash-ref schema.json 'info)))
;; (pretty-print (list 'externalDocs: (hash-ref schema.json 'externalDocs)))
;; (pretty-print (list 'tags:         (hash-ref schema.json 'tags)))
;; (pretty-print (list 'paths:        (hash-keys (hash-ref schema.json 'paths))))
;; (pretty-print (list 'components:   (hash-keys (hash-ref schema.json 'components))))

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
(define v2:index.js "
window.addEventListener('load', function(){
var find_concepts_form           = document.getElementById('find-concepts-form');
var find_concepts_text           = document.getElementById('find-concepts-text');
var find_concepts_submit         = document.getElementById('find-concepts-submit');
var find_concepts_result         = document.getElementById('find-concepts-result');
var find_concepts_result_clear   = document.getElementById('find-concepts-result-clear');
var find_categories_form         = document.getElementById('find-categories-form');
var find_categories_text         = document.getElementById('find-categories-text');
var find_categories_submit       = document.getElementById('find-categories-submit');
var find_categories_result       = document.getElementById('find-categories-result');
var find_categories_result_clear = document.getElementById('find-categories-result-clear');
var find_predicates_form         = document.getElementById('find-predicates-form');
var find_predicates_text         = document.getElementById('find-predicates-text');
var find_predicates_submit       = document.getElementById('find-predicates-submit');
var find_predicates_result       = document.getElementById('find-predicates-result');
var find_predicates_result_clear = document.getElementById('find-predicates-result-clear');
var query_result       = document.getElementById('query-result');
var query_result_clear = document.getElementById('query-result-clear');
var query_form         = document.getElementById('query-form');
var query_text         = document.getElementById('query-text');
var query_submit       = document.getElementById('query-submit');
function pretty_json(json_text) {
  try { return JSON.stringify(JSON.parse(json_text), null, 2); }
  catch (_) { return json_text; }
}
function show(element, result) { element.textContent = pretty_json(result); }

function find_concepts_show(result)     { show(find_concepts_result,   result); }
function find_categories_show(result)   { show(find_categories_result, result); }
function find_predicates_show(result)   { show(find_predicates_result, result); }
function find_concepts_clear_result()   { find_concepts_result.textContent = ''; }
function find_categories_clear_result() { find_categories_result.textContent = ''; }
function find_predicates_clear_result() { find_predicates_result.textContent = ''; }
function find_concepts(data) {
  var xhr = new XMLHttpRequest();
  xhr.addEventListener('load',  function(event){ find_concepts_show(xhr.responseText); });
  xhr.addEventListener('error', function(event){ find_concepts_show('POST error'); });
  xhr.open('POST', '/v2/find-concepts');
  xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
  xhr.send(data);
}
function find_categories(data) {
  var xhr = new XMLHttpRequest();
  xhr.addEventListener('load',  function(event){ find_categories_show(xhr.responseText); });
  xhr.addEventListener('error', function(event){ find_categories_show('POST error'); });
  xhr.open('POST', '/v2/find-categories');
  xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
  xhr.send(data);
}
function find_predicates(data) {
  var xhr = new XMLHttpRequest();
  xhr.addEventListener('load',  function(event){ find_predicates_show(xhr.responseText); });
  xhr.addEventListener('error', function(event){ find_predicates_show('POST error'); });
  xhr.open('POST', '/v2/find-predicates');
  xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
  xhr.send(data);
}
find_concepts_form.addEventListener('submit', function(event){
  event.preventDefault();
  find_concepts('\"'+find_concepts_text.value+'\"');
}, false);
find_categories_form.addEventListener('submit', function(event){
  event.preventDefault();
  find_categories('\"'+find_categories_text.value+'\"');
}, false);
find_predicates_form.addEventListener('submit', function(event){
  event.preventDefault();
  find_predicates('\"'+find_predicates_text.value+'\"');
}, false);
find_concepts_result_clear.addEventListener('click', function(){
  find_concepts_clear_result();
}, false);
find_categories_result_clear.addEventListener('click', function(){
  find_categories_clear_result();
}, false);
find_predicates_result_clear.addEventListener('click', function(){
  find_predicates_clear_result();
}, false);

function query(data) {
  var xhr = new XMLHttpRequest();
  xhr.addEventListener('load',  function(event){ query_show(xhr.responseText); });
  xhr.addEventListener('error', function(event){ query_show('POST error'); });
  xhr.open('POST', '/v2/query');
  xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
  xhr.send(data);
}
function query_show(result)   { show(query_result, result); }
function query_clear_result() { query_result.textContent = ''; }
query_text.textContent = pretty_json(query_text.textContent);
query_form.addEventListener('submit', function(event){
  event.preventDefault();
  query(query_text.value);
}, false);
query_result_clear.addEventListener('click', function(){
  query_clear_result();
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
      \"nodes\": {
        \"n0\": { \"id\": \"UMLS:C0935989\" },
        \"n1\": { \"category\": \"gene\" },
        \"n2\": { \"id\": \"UMLS:C0004096\" }
      },
      \"edges\": {
        \"e0\": {
          \"predicate\": \"negatively_regulates\",
          \"subject\": \"n0\",
          \"object\": \"n1\"
        },
        \"e1\": {
          \"predicate\": \"gene_associated_with_condition\",
          \"subject\": \"n1\",
          \"object\": \"n2\"
        }
      }
    }
  }
}"
                            ))
                     (div (button ((type "submit") (id "query-submit"))
                                  "POST /query")))
               (div (button ((id "query-result-clear")) "Clear Result"))
               (div (pre ((id "query-result")) "Result will appear here.")))))

;; (define v2:index.html
;;   `(html (head (title "mediKanren Reasoner API")
;;                (script ((src "/v2/index.js"))))
;;          (body (h1 "mediKanren Reasoner API")
;;                (p (a ((href "https://github.com/NCATS-Tangerine/NCATS-ReasonerStdAPI"))
;;                      "NCATS Biomedical Translator Reasoners Standard API"))
;;                (ul (li (a ((href "/schema.html")) "schema.html"))
;;                    (li (a ((href "/schema.html2")) "schema.html2"))
;;                    (li (a ((href "/schema.yaml")) "schema.yaml"))
;;                    (li (a ((href "/schema.json")) "schema.json")))
;;                (form ((method "post") (id "find-concepts-form"))
;;                      (div (input ((type "text")
;;                                   (id "find-concepts-text")
;;                                   (value "UMLS:C0935989"))))
;;                      (div (button ((type "submit") (id "find-concepts-submit"))
;;                                   "Find concepts")))
;;                (div (button ((id "find-concepts-result-clear")) "Clear"))
;;                (div (pre ((id "find-concepts-result")) "Concepts will appear here."))
;;                (form ((method "post") (id "find-categories-form"))
;;                      (div (input ((type "text")
;;                                   (id "find-categories-text")
;;                                   (value "gene"))))
;;                      (div (button ((type "submit") (id "find-categories-submit"))
;;                                   "Find categories")))
;;                (div (button ((id "find-categories-result-clear")) "Clear"))
;;                (div (pre ((id "find-categories-result")) "Categories will appear here."))
;;                (form ((method "post") (id "find-predicates-form"))
;;                      (div (input ((type "text")
;;                                   (id "find-predicates-text")
;;                                   (value "negatively_regulates"))))
;;                      (div (button ((type "submit") (id "find-predicates-submit"))
;;                                   "Find predicates")))
;;                (div (button ((id "find-predicates-result-clear")) "Clear"))
;;                (div (pre ((id "find-predicates-result")) "Predicates will appear here."))
;;                (p (a ((href "/predicates")) "GET /predicates"))
;;                (form ((method "post") (action "/v2/query") (id "query-form"))
;;                      (div (textarea
;;                             ((id "query-text"))
;;                             "{
;;   \"message\": {
;;     \"query_graph\": {
;;       \"nodes\": {
;;         \"n0\": { \"id\": \"UMLS:C0935989\" },
;;         \"n1\": { \"category\": \"gene\" },
;;         \"n2\": { \"id\": \"UMLS:C0004096\" }
;;       },
;;       \"edges\": {
;;         \"e0\": {
;;           \"predicate\": \"negatively_regulates\",
;;           \"subject\": \"n0\",
;;           \"object\": \"n1\"
;;         },
;;         \"e1\": {
;;           \"predicate\": \"gene_associated_with_condition\",
;;           \"subject\": \"n1\",
;;           \"object\": \"n2\"
;;         }
;;       }
;;     }
;;   }
;; }"
;;                             ))
;;                      (div (button ((type "submit") (id "query-submit"))
;;                                   "POST /v2/query")))
;;                (div (button ((id "query-result-clear")) "Clear Result"))
;;                (div (pre ((id "query-result")) "Result will appear here.")))))

(define hash-empty (hash))
(define (str   v) (if (string? v) v (error "invalid string:" v)))
(define (olift v) (if (hash?   v) v (error "invalid object:" v)))
(define (slift v) (cond ((pair?   v) v)
                        ((string? v) (list v))
                        ((null?   v) '())
                        (else        (error "invalid string or list of strings:" v))))

(define (alist->attributes alist)
  ;; TODO: provide standard types for
  ;; * negated, publications, provided_by
  ;; * ngd_score, association_type, id
  ;; * update_date, publications_info
  ;; * is_defined_by, pmids, n_pmids, SEMMED_PRED
  (map (lambda (kv) (hash 'name  (car kv)
                          'type  "miscellaneous"
                          'value (cdr kv)))
       alist))

;; (define (concept->result c)
;;   (define attrs
;;     (alist->attributes
;;       (cons (cons "mediKanren-source" (symbol->string (concept->dbname c)))
;;             (concept->props c))))
;;   (cons (string->symbol (concept->curie c))
;;         (hash 'name       (concept->name c)
;;               'category   (cdr (concept->category c))
;;               'attributes attrs)))
;; (define (edge->result e)
;;   (define id (string-append (symbol->string (edge->dbname e)) "."
;;                             (number->string (edge->eid e))))
;;   (define props (make-immutable-hash (edge->props e)))
;;   (define relation (hash-ref props "relation" #f))
;;   (define attrs
;;     (alist->attributes
;;       (hash->list
;;         (foldl (lambda (k ps) (hash-remove ps k)) props
;;                '("relation" "subject" "object"
;;                  "simplified_relation" "simplified_edge_label")))))
;;   (define obj
;;     (hash 'predicate  (cdr (edge->pred e))
;;           'subject    (concept->curie (edge->subject e))
;;           'object     (concept->curie (edge->object e))
;;           'attributes attrs))
;;   (cons (string->symbol id) (if relation
;;                               (hash-set obj 'relation relation)
;;                               obj)))


(define (message->response msg)

  (define broad-response (time (api-query (string-append url.broad path.query)
                                          (hash 'message msg))))
  (define broad-results (hash-ref broad-response 'response))
  (printf "broad response:\n~s\n" (hash-ref broad-response 'status))
  (pretty-print (hash-ref broad-response 'headers))
  (printf "broad result size: ~s\n" (js-count broad-results))
  ;; NOTE: ignore 'results and 'knowledge_graph until we find a use for them.

   (define local-results (trapi-response msg))

  (hash 'results local-results)

  (merge-results
    (list (hash-ref (olift broad-results) 'message hash-empty)
          local-results))

          ;; (hash 'results local-results
          ;;       'knowledge_graph
          ;;       (hash 'nodes (apply hash-union (hash)
          ;;                           (map (lambda (knode) (hash (car knode) (cdr knode)))
          ;;                                knodes)
          ;;                           #:combine
          ;;                           (lambda (c.0 c.1)
          ;;                             (hash-update
          ;;                               c.0 'attributes
          ;;                               (lambda (attrs)
          ;;                                 (append (alist->attributes
          ;;                                           (list (cons "extra-result" c.1)))
          ;;                                         attrs)))))
          ;;             'edges (make-immutable-hash kedges)))))
  )

(define (merge-results rs)
  (let loop ((rs rs) (results '()) (nodes '()) (edges '()))
    (cond ((null? rs) (hash 'results         results
                            'knowledge_graph (hash 'nodes (make-immutable-hash nodes)
                                                   'edges (make-immutable-hash edges))))
          (else (define r (car rs))
                (define kg (hash-ref r 'knowledge_graph hash-empty))
                (loop (cdr rs)
                      (append (hash-ref r 'results '()) results)
                      (append (hash->list (hash-ref kg 'nodes hash-empty))
                              nodes)
                      (append (hash->list (hash-ref kg 'edges hash-empty))
                              edges))))))

;; (define (predicates)
;;   ;; TODO: at greater expense, we could restrict each list of predicates
;;   ;; to those reflected by existing edges for the corresponding categories.
;;   (define cs (run* (c) (categoryo c)))
;;   (define ps (run* (p) (predicateo p)))
;;   (define dbs (map car cs))
;;   (make-immutable-hash
;;     (append*
;;       (map (lambda (db)
;;              (define dcs
;;                (filter-not
;;                  not (map (lambda (c) (and (eq? (car c) db) (cddr c)))
;;                           cs)))
;;              (define dps
;;                (filter-not
;;                  not (map (lambda (p) (and (eq? (car p) db) (cddr p)))
;;                           ps)))
;;              (define dcps
;;                (make-immutable-hash
;;                  (map (lambda (c) (cons (string->symbol c) dps)) dcs)))
;;              (map (lambda (c) (cons (string->symbol c) dcps)) dcs))
;;            dbs))))

;; (define predicates-cached (string->bytes/utf-8 (jsexpr->string (predicates))))
;; (define predicates-cached-gzip (gzip/bytes predicates-cached))

(define (query jsdata)
  (cond ((or (eof-object? jsdata) (not (hash? jsdata))) 'null)
        (else (hash 'message
                    (message->response (olift (hash-ref (olift jsdata) 'message
                                                        hash-empty)))))))
(define (accepts-gzip? req)
  (member "gzip" (map string-trim
                      (string-split (alist-ref (request-headers req)
                                               'accept-encoding "") ","))))
(define (respond code message headers mime-type body)
  (response/full code (string->bytes/utf-8 message)
                 (current-seconds) mime-type headers
                 (list body)))
(define (OK req extra-headers mime-type body (body-gzipped? #f))
  (define gzip? (accepts-gzip? req))
  (define headers (if gzip? (cons (make-header #"Content-Encoding" #"gzip")
                                  extra-headers)
                    extra-headers))
  (define bytes.body (if (string? body) (string->bytes/utf-8 body) body))
  (define payload (if (and gzip? (not body-gzipped?)) (gzip/bytes bytes.body)
                    bytes.body))
  (respond 200 "OK" headers mime-type payload))
(define (not-found req)
  (respond 404 "Not Found" '() mime:html
           (string->bytes/utf-8
             (xexpr->html-string
               (not-found.html (url->string (request-uri req)))))))
(define (OK/jsexpr f req)
  (define input (bytes->jsexpr (request-post-data/raw req)))
  (define result (job (thunk (f input))))
  (if (job-failure? result)
    (respond 400 "Bad Request" '() mime:text
             (string->bytes/utf-8 (job-failure-message result)))
    (OK req '() mime:json (jsexpr->string result))))

;; (define (/index req)
;;   (pretty-print `(request-headers: ,(request-headers req)))
;;   (OK req '() mime:html (xexpr->html-string index.html)))
;; (define (/v2/index req)
;;   (pretty-print `(request-headers: ,(request-headers req)))
;;   (OK req '() mime:html (xexpr->html-string v2:index.html)))
;; (define (/index.js     req) (OK req '() mime:js   index.js))
;; (define (/schema.json  req) (OK req '() mime:text schema.json.txt))
;; (define (/schema.yaml  req) (OK req '() mime:text schema.yaml.txt))
;; (define (/schema.html  req) (OK req '() mime:html schema.html))
;; (define (/schema.html2 req) (OK req '() mime:html schema.html2))
;; (define (/predicates   req) (if (accepts-gzip? req)
;;                               (OK req '() mime:json predicates-cached-gzip #t)
;;                               (OK req '() mime:json predicates-cached #f)))

(define (/query        req) (OK/jsexpr query req))

(define (group-by-db xs)
  (foldl (lambda (x db=>id)
           (hash-update db=>id (car x) (lambda (xs) (cons (cdr x) xs)) '()))
         (hash) xs))

(define (pretty-synonyms ss)
  (if (and (pair? ss) (not (cdar ss))) '()
    (make-immutable-hash
      (map (lambda (kv) (cons (string->symbol (car kv)) (cdr kv))) ss))))

;; (define (find-concepts/any str)
;;   (hash 'synonyms (pretty-synonyms (curie-synonyms/names str))
;;         'concepts
;;         (make-immutable-hash
;;           (hash-map
;;             (group-by-db
;;               (map (lambda (c)
;;                      (define r (concept->result c))
;;                      (define attrs (hash-ref (cdr r) 'attributes))
;;                      (define dbnames
;;                        (map (lambda (attr) (hash-ref attr 'value))
;;                             (filter (lambda (attr) (equal? (hash-ref attr 'name)
;;                                                            "mediKanren-source"))
;;                                     attrs)))
;;                      (define dbname (if (null? dbnames) 'unknown
;;                                       (string->symbol (car dbnames))))
;;                      (cons dbname r))
;;                    (append (find-concepts #t (list str))
;;                            (find-concepts #f (list str)))))
;;             (lambda (db cs) (cons db (make-immutable-hash cs)))))))

;; (define ((find/db-id find) data)
;;   (group-by-db (map (lambda (x) (cons (car x) (cddr x))) (find (list data)))))

;; (define (/v2/index.js        req) (OK req '() mime:js v2:index.js))
(define (/v2/query req)           (OK/jsexpr query                        req))
;; (define (/v2/find-concepts   req) (OK/jsexpr find-concepts/any            req))
;; (define (/v2/find-categories req) (OK/jsexpr (find/db-id find-categories) req))
;; (define (/v2/find-predicates req) (OK/jsexpr (find/db-id find-predicates) req))

(struct job-failure (message))

(define (job work)
  (define job-response (make-channel))
  (channel-put job-request (cons job-response work))
  (channel-get job-response))

(define job-request (make-channel))

(define worker
  (thread
    (thunk (let loop ()
             (match-define (cons job-response work) (channel-get job-request))
             (channel-put job-response
                          (with-handlers ((exn:fail?
                                            (lambda (v)
                                              ((error-display-handler) (exn-message v) v)
                                              (job-failure (exn-message v))))
                                          ((lambda _ #t)
                                           (lambda (v)
                                             (define message
                                               (string-append "unknown error: "
                                                              (with-output-to-string (thunk (write v)))))
                                             (pretty-write message)
                                             (job-failure message))))
                            (work)))
             (loop)))))

(define (start)
  (define-values (dispatch _)
    (dispatch-rules
      ;; (("")                     #:method "get"  /index)
      ;; (("index.js")             #:method "get"  /index.js)
      ;; (("schema.json")          #:method "get"  /schema.json)
      ;; (("schema.yaml")          #:method "get"  /schema.yaml)
      ;; (("schema.html")          #:method "get"  /schema.html)
      ;; (("schema.html2")         #:method "get"  /schema.html2)
      ;; (("predicates")           #:method "get"  /predicates)
      ;; (("query")                #:method "post" /query)
      ;; (("v2")                   #:method "get"  /v2/index)
      ;; (("v2" "index.js")        #:method "get"  /v2/index.js)
      ;; (("v2" "find-concepts")   #:method "post" /v2/find-concepts)
      ;; (("v2" "find-categories") #:method "post" /v2/find-categories)
      ;; (("v2" "find-predicates") #:method "post" /v2/find-predicates)
      (("v2" "query")           #:method "post" /v2/query)
      (else                                     not-found)))
  (serve/servlet dispatch
                 ;; none-manager for better performance:
                 ;; only possible because we're not using web continuations.
                 #:manager (create-none-manager #f)
                 #:servlet-regexp #rx""
                 #:listen-ip #f  ;; comment this to disable external connection
                 #:port 8080
                 #:launch-browser? #f))

(module+ main (start))
