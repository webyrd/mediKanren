#lang racket/base
(require
 "common.rkt"   
  "trapi.rkt"
  "logging.rkt"
  "lw-reasoning.rkt"
  "open-api/api-query.rkt"
  "ingest-pipeline-status.rkt"
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
  web-server/safety-limits
  racket/sandbox
  )

(define query-time-limit (make-parameter 600))

(define (alist-ref alist key default)
  (define kv (assoc key alist))
  (if kv (cdr kv) default))

(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)
(define argv (current-command-line-arguments))
(define argv-optional '#(CONFIG_FILE))
(when (not (<= (vector-length argv) (vector-length argv-optional)))
  (error (format "optional arguments ~s; given ~s" argv-optional argv)))
;; Loading will occur at first use if not explicitly forced like this.
(load-config #t (and (<= 1 (vector-length argv)) (vector-ref argv 0)))
;; (load-databases #t)

(define-runtime-path path:root ".")
(define (path/root relative-path) (build-path path:root relative-path))

(define schema.json.txt
  (file->string (path/root "open-api/TranslatorReasonersAPI.json")))
(define schema.yaml.txt
  (file->string (path/root "open-api/TranslatorReasonersAPI.yaml")))
(define schema.json
  (call-with-input-file (path/root "open-api/TranslatorReasonersAPI.json")
                        read-json))

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
  xhr.open('POST', '/find-concepts');
  xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
  xhr.send(data);
}
function find_categories(data) {
  var xhr = new XMLHttpRequest();
  xhr.addEventListener('load',  function(event){ find_categories_show(xhr.responseText); });
  xhr.addEventListener('error', function(event){ find_categories_show('POST error'); });
  xhr.open('POST', '/find-categories');
  xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
  xhr.send(data);
}
function find_predicates(data) {
  var xhr = new XMLHttpRequest();
  xhr.addEventListener('load',  function(event){ find_predicates_show(xhr.responseText); });
  xhr.addEventListener('error', function(event){ find_predicates_show('POST error'); });
  xhr.open('POST', '/find-predicates');
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
  xhr.open('POST', '/pmi/v2/query');
  xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
  xhr.send(data);
}
function query_show(result)   { show(query_result, result); }
function query_clear_result() { query_result.textContent = ''; }
query_text.textContent = pretty_json(query_text.textContent);
query_form.addEventListener('submit', function(event){
  event.preventDefault();
  query_result.textContent = 'Querying...';
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
               (ul (li (a ((href "/schema.yaml")) "schema.yaml"))
                   (li (a ((href "/schema.json")) "schema.json")))
               (div ((style "display:none"))
                    (form ((method "post") (id "find-concepts-form"))
                          (div (input ((type "text")
                                       (id "find-concepts-text")
                                       (value "UMLS:C0935989"))))
                          (div (button ((type "submit") (id "find-concepts-submit"))
                                       "Find concepts")))
                    (div (button ((id "find-concepts-result-clear")) "Clear"))
                    (div (pre ((id "find-concepts-result")) "Concepts will appear here."))
                    (form ((method "post") (id "find-categories-form"))
                          (div (input ((type "text")
                                       (id "find-categories-text")
                                       (value "gene"))))
                          (div (button ((type "submit") (id "find-categories-submit"))
                                       "Find categories")))
                    (div (button ((id "find-categories-result-clear")) "Clear"))
                    (div (pre ((id "find-categories-result")) "Categories will appear here."))
                    (form ((method "post") (id "find-predicates-form"))
                          (div (input ((type "text")
                                       (id "find-predicates-text")
                                       (value "negatively_regulates"))))
                          (div (button ((type "submit") (id "find-predicates-submit"))
                                       "Find predicates")))
                    (div (button ((id "find-predicates-result-clear")) "Clear"))
                    (div (pre ((id "find-predicates-result")) "Predicates will appear here."))
                    (p (a ((href "/predicates")) "GET /predicates")))
               (form ((method "post") (action "/pmi/v2/query") (id "query-form"))
                     (div (textarea
                            ((id "query-text")(rows "40") (cols "60"))
                            #<<EOS
{
   "message":{
      "query_graph":{
         "nodes":{
            "n0":{
               "ids":["UMLS:C0221347"],
               "categories":["biolink:PhenotypicFeature"]
            },
            "n1":{
               "categories":["biolink:NamedThing"]
            }
         },
         "edges":{
            "e01":{
               "subject":"n0",
               "object":"n1"
            }
         }
      }
   }
}
EOS
         ))
                     (div (button ((type "submit") (id "query-submit"))
                                  "POST /pmi/v2/query")))
               (div (pre ((id "query-result")) "Result will appear here.")))))

(define hash-empty (hash))
(define (str   v) (if (string? v) v (error (format "invalid string:" v))))
(define (olift v) (if (hash?   v) v (error (format "invalid object:" v))))
(define (slift v) (cond ((pair?   v) v)
                        ((string? v) (list v))
                        ((null?   v) '())
                        (else        (error (format "invalid string or list of strings:" v)))))

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
  (define log-key (current-seconds))
  (define broad-response (if (or
                                (hash-ref msg 'disable_external_requests #f)
                                (not (cfg:config-ref 'trapi-enable-external-requests?)))
                             (hash 'response hash-empty 'status "disabled" 'headers '())
                             (time (api-query (string-append url.broad path.query)
                                              (hash 'message msg)))))
  (define broad-results (hash-ref broad-response 'response))
  (define broad-results-count (length (hash-ref (hash-ref broad-results 'message hash-empty) 'results '())))
  (define broad-error-message (hash-ref broad-results 'detail #f)) ; not sure if this is stable - it isn't TRAPI
  (pretty-print (format "Broad response:\n~s\n" (hash-ref broad-response 'status)))
  (log-info log-key (format "Broad response:\n~s\n" (hash-ref broad-response 'status)))
  (pretty-print (format "Headers: ~s\n" (hash-ref broad-response 'headers)))
  (log-info log-key (format "Headers: ~s\n" (hash-ref broad-response 'headers)))
  (pretty-print (format "Broad result size: ~s\n" broad-results-count))
  (log-info log-key (format "Broad result size: ~s\n" broad-results-count))

  ;; (log-info log-key (format "Broad results: ~s" broad-results))
  
  ;; (with-handlers ((exn:fail? (lambda (exn) 
  ;;                              (hash 'error (exn-message exn)))))
  (pretty-print (format "Query received: ~a" (jsexpr->string msg)))
  (log-info log-key (format "Query received: ~a" (jsexpr->string msg)))

  (with-handlers ((exn:fail:resource?
                   (lambda (exn) 
                     (log-error log-key (format "Error: ~a" exn))
                     (error "Max query time exceded"))))
    (call-with-limits (query-time-limit) #f
      (lambda ()
        (let-values (((result cpu real gc) (time-apply (lambda () (trapi-response msg log-key)) '())))
          (let* ((local-results (car result))
                 (length-local (length (hash-ref  local-results 'results '()))))
            (pretty-print (format "Query time [cpu time: ~s real time: ~s]" cpu real))
            (log-info log-key (format "Query time [cpu time: ~s real time: ~s]" cpu real))
            (pretty-print (format "Local results size: ~s" length-local))
            (log-info log-key (format "Local results size: ~s" length-local))
            (values (hash-set*
                     (merge-results
                      (list (hash-ref (olift broad-results) 'message hash-empty)
                            local-results)))
                    (list (hash 'level "INFO"
                                'message (format "Query time: ~ams" cpu))
                          (if broad-error-message
                              (hash 'level "WARNING"
                                    'message (format "MolePro error: ~s" broad-error-message))
                              (hash 'level "INFO"
                                    'message (format "MolePro results: ~a" broad-results-count)))
                          (hash 'level "INFO"
                                'message (format "MolePro response status: ~s" (hash-ref broad-response 'status)))
                          ))))))))
    
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
        (else (let* ((data (olift jsdata))
                     (request-msg (olift (hash-ref data 'message hash-empty))))
                (let-values (((message logs) (message->response request-msg)))
                  (let ((length-local (length (hash-ref message 'results))))
                    (hash 'message message
                          'query_graph (hash-ref request-msg 'query_graph '#hash())
                          'status "Success"
                          'description (format "Success. ~s result~a." length-local (if (= length-local 1) "" "s"))
                          'logs logs)))))))
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

(define (/index req)
  (pretty-print `(request-headers: ,(request-headers req)))
  (OK req '() mime:html (xexpr->html-string index.html)))

(define (/schema.json  req) (OK req '() mime:text schema.json.txt))
(define (/schema.yaml  req) (OK req '() mime:text schema.yaml.txt))
;; (define (/predicates   req) (if (accepts-gzip? req)
;;                               (OK req '() mime:json predicates-cached-gzip #t)
;;                               (OK req '() mime:json predicates-cached #f)))

;; (define (/query        req) (OK/jsexpr query req))

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
(define (/health req)
  (let-values (((result cpu real gc) (time-apply (lambda () (run 1 () (triple "NCIT:C18585" "biolink:actively_involved_in" "NCIT:C45399"))) '())))
    (let ((response
           (if (null? result)
               (hash 'status "corrupt"
                     'reason "no data"
                     'real_time real
                     'cpu_time cpu)
               (hash 'status "online"
                     'real_time real
                     'cpu_time cpu))))
   (OK req '() mime:json (jsexpr->string response)))))

(define (/index.js req)  (OK req '() mime:js index.js))
;; (define (/health req)    (OK health                        req))
(define (/query req)
  (pretty-print `(request-headers: ,(request-headers req)))
  (OK/jsexpr query                        req))
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
     (("")                     #:method "get"  /index)
     (("index.js")             #:method "get"  /index.js)
     (("schema.json")          #:method "get"  /schema.json)
     (("schema.yaml")          #:method "get"  /schema.yaml)
     ;; (("predicates")           #:method "get"  /predicates)

     ;; Legacy endpoint - unversioned - TRAPI 1.0 - TODO: Delete
     (("query")                #:method "post" /query)

     ;; Current (versioned) endpoint for NCATS - TRAPI 1.1
     (("v2" "query")     #:method "post" /query)

     ;; Endpoint for Andy's webapp
     (("pmi" "v2" "query")     #:method "post" /query)
     ;; TODO: restore text search
     ;; (("pmi" "v2" "query" "find-concepts")   #:method "post" /v2/find-concepts)
     ;; (("pmi" "v2" "query" "find-categories") #:method "post" /v2/find-categories)
     ;; (("pmi" "v2" "query" "find-predicates") #:method "post" /v2/find-predicates)

     (("pmi" "v2" "ingest-pipeline" "status") #:method "get" /ingest-pipeline-status)

     (("health")               #:method "get" /health)

     (else                                     not-found)))
  (serve/servlet dispatch
                 ;; none-manager for better performance:
                 ;; only possible because we're not using web continuations.
                 #:manager (create-none-manager #f)
                 #:servlet-regexp #rx""
                 #:listen-ip #f  ;; comment this to disable external connection
                 #:port 8384
                 #:launch-browser? #f
                 #:safety-limits (make-safety-limits #:response-send-timeout 6000
                                                     #:response-timeout 6000)
                 ))

(module+ main (start))
