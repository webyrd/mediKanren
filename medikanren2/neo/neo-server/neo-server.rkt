#lang racket/base

(provide serve
         DEFAULT_PORT)
(require
 "../../logging2.rkt"
 "../neo-low-level/query-low-level-multi-db.rkt"
 "../neo-open-api/neo-api-query.rkt"
 "../neo-reasoning/neo-biolink-reasoning.rkt"
 "../neo-utils/neo-helpers-multi-db.rkt"
 racket/file
 racket/match
 racket/set
 racket/port
 racket/pretty
 racket/runtime-path
 racket/string
 racket/tcp
 json
 xml
 net/url)

;; Team Unsecret Agent mediKanren 2 neo server

(define DEFAULT_PORT 8384)

(define NEO_SERVER_VERSION "1.0")

;; Maximum number of results to be returned from *each individual* KP,
;; or from mediKanren itself.
(define MAX_RESULTS_FROM_COMPONENT 250)

;; Number of seconds before a connection times out, collecting all
;; resources from the connection (was 10 seconds in the original
;; tutorial).
(define CONNECTION_TIMEOUT_SECONDS (* 10 60))
(define API_CALL_CONNECTION_TIMEOUT_SECONDS (* 1 60))

;; Per-servelet memory limit (due to garbage collection overhead,
;; actual RAM usage can be a small multiple of this amount)
(define SERVELET_MEMORY_USAGE_LIMIT (* 50 1024 1024))


;; ** `tcp-listen` settings **
;;
;; From the Racket documentation for `tcp-listen` in
;; https://docs.racket-lang.org/reference/tcp.html:
;;
;; "The max-allow-wait argument determines the maximum number of
;; client connections that can be waiting for acceptance. (When
;; max-allow-wait clients are waiting acceptance, no new client
;; connections can be made.)"
(define MAX_ALLOW_WAIT 5)
;; "If the reuse? argument is true, then tcp-listen will create a
;; listener even if the port is involved in a TIME_WAIT state. Such a
;; use of reuse? defeats certain guarantees of the TCP protocol; see
;; Stevens’s book for details. Furthermore, on many modern platforms,
;; a true value for reuse? overrides TIME_WAIT only if the listener
;; was previously created with a true value for reuse?."
(define REUSE_LISTENER_WHILE_IN_TIME_WAIT_STATE #f)
;; "If hostname is #f (the default), then the listener accepts
;; connections to all of the listening machine’s addresses. Otherwise,
;; the listener accepts connections only at the interface(s)
;; associated with the given hostname. For example, providing
;; "127.0.0.1" as hostname creates a listener that accepts only
;; connections to "127.0.0.1" (the loopback interface) from the local
;; machine."
(define ONLY_ACCEPT_CONNECTIONS_FROM_GIVEN_HOSTNAME #f)

(define MAX_POST_REQUEST_CONTENT_LENGTH 100000)

;; HTTP header strings
(define HTTP_VERSION_STRING "HTTP/1.0")
(define SERVER_STRING "Server: k")
(define 200_OK_STRING "200 OK")
(define 500_ERROR_STRING "500 Internal Server Error")

(define-runtime-path path:root ".")
(define (path/root relative-path) (build-path path:root relative-path))

(define schema.json.txt
  (file->string (path/root "../neo-open-api/mediKanrenSmartAPI.json")))
(define schema.yaml.txt
  (file->string (path/root "../neo-open-api/mediKanrenSmartAPI.yaml")))
(define metaKG.json.txt
  (file->string (path/root "../neo-open-api/unsecret_metaKG.json")))


(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener
      (tcp-listen port-no
                  MAX_ALLOW_WAIT
                  REUSE_LISTENER_WHILE_IN_TIME_WAIT_STATE
                  ONLY_ACCEPT_CONNECTIONS_FROM_GIVEN_HOSTNAME))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (define shut-down-already-attempted #f)
  (lambda ()
    (when shut-down-already-attempted
      (printf "already tried to shut down this server!  Trying again...\n")
      (if (custodian-shut-down? main-cust)
          (printf "main custodian already shut down.  Trying again...\n")
          (printf "*** weird---main custodian is still active!  Trying again...\n")))
    (printf "shutting down server...\n")
    (custodian-shutdown-all main-cust)
    (set! shut-down-already-attempted #t)
    (printf "server shut down\n")))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (custodian-limit-memory cust SERVELET_MEMORY_USAGE_LIMIT)
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (printf "\ntcp-accept accepted connection\n")
    (thread
     (lambda ()
       (printf "\n++ started handle thread for Neo Server ~a ++\n"
               NEO_SERVER_VERSION)
       (handle in out
               (lambda ()
                 (printf "** connection failure continuation invoked!\n")
                 (custodian-shutdown-all cust))
               (lambda ()
                 (printf "** request failure continuation invoked!\n")
                 (custodian-shutdown-all cust)))
       (close-input-port in)
       (close-output-port out)
       (printf "handle thread ending\n")
       (custodian-shutdown-all cust)
       ))
    ;; watcher thread:
    (thread (lambda ()
              (sleep CONNECTION_TIMEOUT_SECONDS)
              (printf
               "** watcher thread timed out connection after ~s seconds!\n"
               CONNECTION_TIMEOUT_SECONDS)
              (custodian-shutdown-all cust)))))

(define (get-request-headers in)
  (define request-headers (make-hash))
  (printf "getting request headers\n")
  (let loop ()
    (define current-line-match (regexp-match #rx"[^\r\n]*\r\n" in))
    (if (not current-line-match)
        (begin
          (printf "** error parsing request headers: current line doesn't end properly\n")
          #f)
        (let ((current-line (list-ref current-line-match 0)))
          (printf "current-line:\n~s\n" current-line)
          (cond
            [(regexp-match #rx"^([^:]+:) (.+)\r\n" current-line)
             =>
             (lambda (header)
               (printf "== header:\n~s\n" header)
               (printf "== (list-ref header 1):\n~s\n" (list-ref header 1))
               (printf "== (list-ref header 2):\n~s\n" (list-ref header 1))
               (hash-set! request-headers
                          (bytes->string/utf-8 (list-ref header 1))
                          (bytes->string/utf-8 (list-ref header 2)))
               (printf "== request-headers:\n~s\n\n" request-headers)
               (loop))]
            [(regexp-match #px"^[[:space:]]*\r\n" current-line)
             (printf "parsed request headers:\n~s\n" request-headers)
             request-headers]
            [else
             (printf "** error parsing request headers: ~s\n" current-line)
             #f])))))

(define (get-request-body in content-length)
  (define bytes (read-bytes content-length in))
  (if (eof-object? bytes)
      #f
      (bytes->string/utf-8 bytes)))

(define (send-reply dispatch-result out)
  (begin
    (match dispatch-result
      [`(text ,respose-code ,text)
       (printf "sending text reply\n")
       (display HTTP_VERSION_STRING out)
       (display " " out)
       (display respose-code out)
       (display "\r\n" out)
       (display SERVER_STRING out)
       (display "\r\n" out)
       (display "Content-Type: text/plain\r\n\r\n" out)
       (display text out)]
      [`(json ,respose-code ,jsexpr)
       (printf "sending JSON reply\n")
       (display HTTP_VERSION_STRING out)
       (display " " out)
       (display respose-code out)
       (display "\r\n" out)
       (display SERVER_STRING out)
       (display "\r\n" out)
       (display "Content-Type: application/json\r\n\r\n" out)
       (pretty-print-json-string (jsexpr->string jsexpr) out)]
      [`(xexpr ,respose-code ,xexpr)
       (printf "sending HTML reply\n")
       (display HTTP_VERSION_STRING out)
       (display " " out)
       (display respose-code out)
       (display "\r\n" out)
       (display SERVER_STRING out)
       (display "\r\n" out)
       (display "Content-Type: text/html\r\n\r\n" out)
       (display (xexpr->string xexpr) out)]
      [else
       (printf "unknown dispatch-result type:\n~s\n" dispatch-result)
       (display HTTP_VERSION_STRING out)
       (display " " out)
       (display 500_ERROR_STRING out)
       (display "\r\n" out)])
    (flush-output out)))

;; Try to get the value for a given key from the hash table of
;; headers.  This is a little subtle, since the header keys are case
;; insensitive, yet hash table lookup is case senstitive.  This
;; function returns #f if no entry could be found for the given key
;; string (in a case-insensitive manner).  Otherwise, the function
;; returns the value.
(define (get-key/value-from-headers key-name-str req-headers-hash)
  (define keys (hash-keys req-headers-hash))
  (define lowercase/original-keys
    (map (lambda (key) (cons (string-downcase key) key)) keys))
  (define pr (assoc (string-downcase key-name-str) lowercase/original-keys))
  (if pr
      (let ((actual-key (cdr pr)))
        (let ((value (hash-ref req-headers-hash actual-key)))
          value))
      #f))

;; `conn-fk` is a failure continuation to be invoked upon unexpected
;; closing of the TCP connection, or due to a networking error.  The
;; failure continuation should not be invoked for request that can not
;; be handled due to an unknown request type or to a failure to
;; respond to the request that can be handled gracefully (with a
;; response error code and message).
;;
;; `request-fk` is a failure continuation to be invoked if the request
;; is malformed, incomplete, or non-sensical, such as a request not
;; being parsable, or a `query` POST not containing `Content-Type` or
;; `Content-Length` information, `Content-Length` that isn't legal, or
;; a body that can't be parsed as JSON.
(define (handle in out conn-fk request-fk)
  (define first-input-line (read-line in))
  (printf "handling request:\n~s\n" first-input-line)

  (define get/post-req
    (regexp-match #rx"^(GET|POST) (.+) HTTP/[0-9]+\\.[0-9]+"
                  first-input-line))

  (when get/post-req
    (let ((request-type (list-ref get/post-req 1))
          (str-path (list-ref get/post-req 2)))
      (define req-headers (get-request-headers in))
      (when req-headers
        ;; Detect half-closed TCP connections with eof-evt on the input port
        (with-handlers
            ((exn:fail:network:errno?
              (lambda (ex)
                (cond
                  ((equal? '(110 . posix) (exn:fail:network:errno-errno ex))
                   (display "Error: TCP keepalive failed.  Presuming half-open TCP connection.\n")
                   (conn-fk))
                  (else
                   (printf "Error: Unknown network error ~a.\n" ex)
                   (conn-fk))))))
          ;; watcher thread checking for half-closed TCP connection
          (thread
           (lambda ()
             (let loop-forever ()
               (let ((evt (sync/timeout 1 (eof-evt in))))
                 (cond
                   (evt
                    (displayln "Error: Detected half-closed TCP connection.")
                    (conn-fk))
                   (else
                    (display "." (current-output-port))
                    (flush-output (current-output-port))
                    (loop-forever)))))))

          (match request-type
            ["GET"
             ;; Dispatch GET request:
             (let ([dispatch-result (dispatch-request 'GET
                                                      str-path
                                                      req-headers
                                                      request-fk)])
               (printf "dispatch-result:\n~s\n" dispatch-result)

               ;; Send reply:
               (send-reply dispatch-result out)

               (custodian-shutdown-all (current-custodian))
               )]
            ["POST"
             (printf "handling POST request\n")

             (printf "req-headers:\n~s\n" req-headers)

             (define content-length-string
               (get-key/value-from-headers "Content-Length:" req-headers))

             (unless content-length-string
               (printf "** error: POST request doesn't include 'Content-Length:'\n")
               (request-fk))

             (define content-length (string->number content-length-string))
             (printf "Content-Length as a number:\n~s\n" content-length)

             (unless (and (integer? content-length)
                          (<= 0 content-length)
                          (<= content-length MAX_POST_REQUEST_CONTENT_LENGTH))
               (printf "** error: bad POST content length: ~s\n" content-length)
               (request-fk))

             (define content-type-string
               (get-key/value-from-headers "Content-Type:" req-headers))

             (unless content-type-string
               (printf "** error: POST request doesn't include 'Content-Type:'\n")
               (request-fk))

             (printf "Content-Type:\n~s\n" content-type-string)

             (define body-str (get-request-body in content-length))
             (printf "body-str:\n~s\n" body-str)

             (unless body-str
               (printf "** error: unable to get the body of POST request\n")
               (request-fk))

             (let ([dispatch-result (dispatch-request 'POST
                                                      str-path
                                                      req-headers
                                                      request-fk
                                                      ;;
                                                      content-type-string
                                                      content-length-string
                                                      body-str)])
               ;; (printf "dispatch-result:\n~s\n" dispatch-result)

               ;; Send reply:
               (send-reply dispatch-result out)

               (custodian-shutdown-all (current-custodian))
               )]))))))

;; dispatch for HTTP GET and POST requests
(define (dispatch-request request-type
                          str-path
                          req-headers
                          request-fk
                          . rest-args)
  ;; Parse the request as a URL:
  (define url (string->url str-path))
  ;; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ;; Find a handler based on the path's first element:
  (define dispatch-key (list request-type (car path)))
  (define h (hash-ref dispatch-table dispatch-key #f))
  (printf "dispatch-key: ~s\n" dispatch-key)
  (printf "dispatch-table: ~s\n" dispatch-table)
  (printf "h: ~s\n" h)
  (printf "url: ~s\n" url)
  (printf "path: ~s\n" path)
  (printf "url-query: ~s\n" (url-query url))
  (printf "req-headers: ~s\n" req-headers)
  (printf "rest-args: ~s\n" rest-args)
  (newline)

  (if h
      ;; Call a handler:
      (apply h (url-query url) req-headers request-fk rest-args)
      ;; No handler found:
      (list
        'xexpr
        `(html (head (title "Error"))
               (body
                (font ((color "red"))
                      "Unknown page: "
                      ,str-path))))))


(define (mvp-creative-query? edges nodes)

  (define edge* (hash->list edges))
  (printf "edge*: ~s\n" edge*)

  (match edge*
    ;; exactly one edge
    [`((,edge-id . ,edge-hash))
     (define knowledge_type (hash-ref edge-hash 'knowledge_type #f))

     (printf "knowledge_type: ~s\n" knowledge_type)

     (and (equal? "inferred" knowledge_type)
          (let ()
            (define predicates (hash-ref edge-hash 'predicates #f))
            (printf "predicates: ~s\n" predicates)
            (and (list? predicates)
                 (member "biolink:treats" predicates)
                 (let ()
                   (define subject-id (hash-ref edge-hash 'subject #f))
                   (define object-id (hash-ref edge-hash 'object #f))

                   (define subject-node (hash-ref nodes (string->symbol subject-id) #f))
                   (define object-node (hash-ref nodes (string->symbol object-id) #f))

                   (printf "subject-node: ~s\n" subject-node)
                   (printf "object-node: ~s\n" object-node)

                   (and subject-node
                        object-node
                        (let ()

                          (define object-ids (hash-ref object-node 'ids #f))
                          (printf "object-ids: ~s\n" object-ids)

                          (and (list? object-ids)
                               (not (null? object-ids))
                               (let ()

                                 (define subject-categories (hash-ref subject-node 'categories #f))
                                 (printf "subject-categories: ~s\n" subject-categories)

                                 (and (list? subject-categories)
                                      (member "biolink:ChemicalEntity" subject-categories)
                                      (let ()
                                        (define object-categories (hash-ref object-node 'categories #f))
                                        (printf "object-categories: ~s\n" object-categories)

                                        (or (not object-categories)
                                            (and (list? object-categories)
                                                 (member "biolink:Disease" object-categories)))))))))))))]
    [else #f]))

(define (make-empty-trapi-response body-json)
  (let* ((message (hash-ref body-json 'message #f))
         (query_graph (hash-ref message 'query_graph #f)))

    (hash
      'message
      (hash
        ;;
        'query_graph
        query_graph
        ;;
        'knowledge_graph
        (hash 'nodes (hash)
              'edges (hash))
        ;;
        'results
        '()
       ))))

(define (make-score-result num-results)
  (lambda (result index)
    (- num-results index)))

(define (score-results results)
  (let ((n (length results)))
    (let ((score-one-result (make-score-result n)))
      (map (lambda (h i) (hash-set h 'score (score-one-result h i))) results (iota n)))))

(define unsecret-provenance-attribute
  (hash
   'attribute_source "infores:unsecret-agent"
   'attribute_type_id "biolink:aggregator_knowledge_source"
   'description "The Unsecret Agent mediKanren 2.0 ARA from NCATS Translator"
   'value "infores:unsecret-agent"
   'value_type_id "biolink:InformationResource"
   'value_url "https://medikanren-trapi.ci.transltr.io"
   ))

(define (data-attribute infores)
  (hash
   'attribute_source infores
   ;; TODO: what should go here?
   'attribute_type_id "biolink:aggregator_knowledge_source"
   'value infores
   ;; TODO: what should go here?
   'value_type_id "biolink:InformationResource"
   ))

(define (get-assoc k m)
  (let ((r (assoc k m)))
    (if r
        (cadr r)
        #f)))

(define (list-assoc k m)
  (let ((r (assoc k m)))
    (if r
        (cdr r)
        '())))

(define (num-pubs props)
  (let ((pubs (get-assoc "publications" props)))
    (if pubs
        (length (string-split pubs "|"))
        0)))

(define (normalize-scores results)
  (if (null? results)
      results
      (let ((max-score (hash-ref (car results) 'score)))
        (map (lambda (x) (hash-set x 'score (/ (hash-ref x 'score) (* 1.0 max-score)))) results))))

(define (merge-list xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (merge-list ys (cdr xs)))))

(define (merge-hash h1 h2)
  (define h h2)
  (hash-for-each h1 (lambda (k v) (set! h (hash-set h k v))))
  h)

(define (merge-trapi-responses r1 r2 original-query_graph)
  (let* ((message1 (hash-ref r1 'message))
         (message2 (hash-ref r2 'message))
         (knowledge_graph1 (hash-ref message1 'knowledge_graph))
         (knowledge_graph2 (hash-ref message2 'knowledge_graph))
         (nodes1 (hash-ref knowledge_graph1 'nodes))
         (nodes2 (hash-ref knowledge_graph2 'nodes))
         (edges1 (hash-ref knowledge_graph1 'edges))
         (edges2 (hash-ref knowledge_graph2 'edges))
         (results1 (hash-ref message1 'results))
         (results2 (hash-ref message2 'results)))
    ;; POSSIBLE TODO
    ;; Might want to check that 'original-query_graph'
    ;; is 'equal?' to the 'query_graph' in 'r1' and the
    ;; 'query_graph' in 'r2' (to ensure we aren't trying
    ;; to merge a response that modifies the 'query_graph'
    ;; in creative mode, for example).
    (hash 'message
          (hash
            ;;
            'query_graph
            original-query_graph
            ;;
            'knowledge_graph
            (hash
              'edges (merge-hash edges1 edges2)
              ;;
              'nodes (merge-hash nodes1 nodes2))
            ;;
            'results
            (merge-list results1 results2)
            ))))

(define (handle-mvp-creative-query body-json message query_graph edges nodes)

  (printf "++ handling MVP mode creative query\n")

  (define disable-external-requests
    (hash-ref message 'disable_external_requests #f))

  (define our-trapi-response
    (let ()

      (define query_graph (hash-ref message 'query_graph))
      (define qg_nodes (hash-ref query_graph 'nodes))
      (define qg_edges (hash-ref query_graph 'edges))
      (define qg_edge* (hash->list qg_edges))

      (define qg_edge-hash
        (match qg_edge*
          ;; exactly one edge
          [`((,edge-id . ,edge-hash))
           edge-hash]))

      (define qg_object-node-str (hash-ref qg_edge-hash 'object))
      (define qg_object-node-id (string->symbol qg_object-node-str))

      (define disease-ids
        ;; TODO write a chainer in utils, and also check for errors
        (hash-ref (hash-ref qg_nodes qg_object-node-id) 'ids))

      ;;
      (define q1
        (let ((q1
               ;; TODO
               ;;
               ;; * ensure all of the biolink curies are supported in
               ;; the current biolink standard, or replace
               ;;
               ;; * use qualified predicates
               (query:X->Y->Known
                ;; X
                (set->list
                 (get-non-deprecated-mixed-ins-and-descendent-classes*-in-db
                  '("biolink:ChemicalEntity")))
                (set->list
                 (set-union
                  (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
                   '("biolink:regulates"))
                  (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
                   '("biolink:entity_regulates_entity"))))
                ;; Y
                (set->list
                 (set-union
                  (get-non-deprecated-mixed-ins-and-descendent-classes*-in-db
                   '("biolink:Gene"))
                  (get-non-deprecated-mixed-ins-and-descendent-classes*-in-db
                   '("biolink:GeneOrGeneProduct"))
                  (get-non-deprecated-mixed-ins-and-descendent-classes*-in-db
                   '("biolink:Protein"))))
                (set->list
                 (set-union
                  (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
                   '("biolink:causes"))
                  (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
                   '("biolink:gene_associated_with_condition"))))
                ;;
                (set->list
                 (get-descendent-curies*-in-db
                  (curies->synonyms-in-db disease-ids))))))
          (take-at-most q1 MAX_RESULTS_FROM_COMPONENT)))

      (define nodes (make-hash))

      (define edges (make-hash))

      (define results '())

      (define (add-node! curie)
        (let ((props (curie->properties curie)))
          (let ((categories (list-assoc "category" props))
                (name (get-assoc "name" props)))
            (hash-set! nodes (string->symbol curie)
                       (hash 'categories categories
                             'name name)))))

      (define (add-edge! props)
        ;; TODO: using the id as the edge id might break with
        ;;       other knowledge graphs
        (let ((id (get-assoc "id" props)))
          (hash-set! edges (string->symbol id)
                     (hash 'attributes
                           (list
                             unsecret-provenance-attribute
                             (data-attribute (get-assoc "knowledge_source" props))
                             )
                           'object (get-assoc "object" props)
                           'predicate (get-assoc "predicate" props)
                           'subject (get-assoc "subject" props)))
          id))

      (define (add-result! r)
        (set! results (cons r results)))

      (for-each
        (lambda (e)
          (match e
            [`(,curie_x
               ,name_x
               ,pred_xy
               ,curie_y
               ,name_y
               ,pred_yz
               ,curie_z
               ,name_z
               ,props_xy
               ,props_yz)
             (add-node! curie_x)
             (add-node! curie_y)
             (add-node! curie_z)
             (define edge_xy (add-edge! props_xy))
             (define edge_yz (add-edge! props_yz))
             (add-result!
              (hash 'edge_bindings
                    (hash 'drug_gene (list (hash 'id edge_xy))
                          'gene_dise (list (hash 'id edge_yz)))
                    'node_bindings
                    (hash 'disease (list (hash 'id curie_z))
                          'drug    (list (hash 'id curie_x))
                          'gene    (list (hash 'id curie_y)))
                    ;; TODO: we should downvote any answer that is already in 1-hop
                    'score
                    (* (num-pubs props_xy) (num-pubs props_yz))))
             ]))
        q1)

      (set! results (sort results (lambda (a b) (> (hash-ref a 'score) (hash-ref b 'score)))))

      (hash
        'message
        (hash
          ;;
          'query_graph
          query_graph
          ;;
          'knowledge_graph
          (hash 'edges edges
                'nodes nodes)
          ;;
          'results
          (normalize-scores results)))
      ))

  (define gp-trapi-response
    (if disable-external-requests
        #f
        (let ()

          (define res #f)

          (printf "making sync/timeout API call with timeout of ~s seconds\n"
                  API_CALL_CONNECTION_TIMEOUT_SECONDS)

          (sync/timeout
            API_CALL_CONNECTION_TIMEOUT_SECONDS
            (thread
              (lambda ()
                ;; TODO use 'url.genetics.prod', 'url.genetics.test', or 'url.genetics.ci'
                ;; based on the environment Unsecret server is running in.
                ;;
                ;; Hard-code to use CI for now.
                (set! res
                      (api-query (string-append url.genetics.ci path.query)
                                 body-json)))))

          (if res
              (printf "API call returned\n")
              (printf "API call timed out\n"))

          (if (hash? res)
              (let ()
                (define upstream-status
                  (hash-ref res 'status #f))

                (printf "status from API call:\n~s\n" upstream-status)

                (if upstream-status
                    (let ()
                      (define upstream-headers
                        (hash-ref res 'headers #f))

                      (printf "headers from API call:\n~s\n" upstream-headers)

                      (if (string-contains?
                            (bytes->string/utf-8 upstream-status)
                            200_OK_STRING)
                          (let ()
                            (printf "API returned an OK status...processing results\n")

                            (define upstream-response
                              (hash-ref res 'response #f))

                            (define res-message
                              (hash-ref upstream-response 'message))

                            (define results
                              (let ((results
                                     (hash-ref res-message 'results)))
                                (take-at-most results MAX_RESULTS_FROM_COMPONENT)))

                            (define scored-results
                              (score-results results))

                            (define knowledge_graph
                              (hash-ref res-message 'knowledge_graph))

                            (define edges
                              (hash-ref knowledge_graph 'edges))

                            (define stamped-edges
                              (hash-map/copy
                               edges
                               (lambda (k v)
                                 (values
                                  k
                                  (hash-set v
                                            'attributes
                                            (cons unsecret-provenance-attribute
                                                  (hash-ref v 'attributes)))))))

                            (define stamped-knowledge_graph
                              (hash-set knowledge_graph 'edges stamped-edges))

                            (hash-set upstream-response
                                      'message
                                      (hash-set (hash-set res-message
                                                          'results
                                                          (normalize-scores scored-results))
                                                'knowledge_graph
                                                stamped-knowledge_graph)))
                          (begin
                            (printf "API returned a non-OK status...ignoring results\n")
                            #f)))
                    #f))
              #f))))

  (define trapi-response
    (if gp-trapi-response
        (merge-trapi-responses our-trapi-response gp-trapi-response query_graph)
        our-trapi-response))

  (define merged-results
    (hash-ref (hash-ref trapi-response 'message) 'results))

  (define merged-scored-results
    (normalize-scores (score-results merged-results)))

  (define scored-trapi-response
    (hash-set trapi-response 'message
              (hash-set (hash-ref trapi-response 'message)
                        'results merged-scored-results)))
  (list
    'json
    200_OK_STRING
    scored-trapi-response)
  )


(define (handle-trapi-query body-json request-fk)

  (define message (hash-ref body-json 'message #f))
  (printf "message:\n~s\n" message)
  (unless message
    (printf "** missing `message` in `body-json`: ~s\n" body-json)
    (request-fk))

  (define query_graph (hash-ref message 'query_graph #f))
  (printf "query_graph:\n~s\n" query_graph)
  (unless query_graph
    (printf "** missing `query_graph` in `message`: ~s\n" message)
    (request-fk))

  (define edges (hash-ref query_graph 'edges #f))
  (printf "edges:\n~s\n" edges)
  (unless edges
    (printf "** missing `edges` in `query_graph`: ~s\n" query_graph)
    (request-fk))

  (define nodes (hash-ref query_graph 'nodes #f))
  (printf "nodes:\n~s\n" nodes)
  (unless nodes
    (printf "** missing `nodes` in `query_graph`: ~s\n" query_graph)
    (request-fk))

  (define creative-mvp? (mvp-creative-query? edges nodes))
  (printf "creative-mvp?: ~s\n" creative-mvp?)

  (if creative-mvp?
      (handle-mvp-creative-query body-json message query_graph edges nodes)
      (let ()

        (printf "-- handling non-MVP mode query\n")

        (define trapi-response
          (make-empty-trapi-response body-json))

        (list
          'json
          200_OK_STRING
          trapi-response)
        )
      )
  )

(define (handle-trapi-asyncquery body-json request-fk)

  (define message (hash-ref body-json 'message #f))
  (printf "message:\n~s\n" message)
  (unless message
    (printf "** missing `message` in `body-json`: ~s\n" body-json)
    (request-fk))

  (define query_graph (hash-ref message 'query_graph #f))
  (printf "query_graph:\n~s\n" query_graph)
  (unless query_graph
    (printf "** missing `query_graph` in `message`: ~s\n" message)
    (request-fk))

  (define edges (hash-ref query_graph 'edges #f))
  (printf "edges:\n~s\n" edges)
  (unless edges
    (printf "** missing `edges` in `query_graph`: ~s\n" query_graph)
    (request-fk))

  (define nodes (hash-ref query_graph 'nodes #f))
  (printf "nodes:\n~s\n" nodes)
  (unless nodes
    (printf "** missing `nodes` in `query_graph`: ~s\n" query_graph)
    (request-fk))

  (define creative-mvp? (mvp-creative-query? edges nodes))
  (printf "creative-mvp?: ~s\n" creative-mvp?)

  (if creative-mvp?
      (handle-mvp-creative-query body-json message query_graph edges nodes)
      (let ()

        (printf "-- handling non-MVP mode query\n")

        (define trapi-response
          (make-empty-trapi-response body-json))

        (list
          'json
          200_OK_STRING
          trapi-response)
        )
      )
  )




(define dispatch-table (make-hash))

;; servlet stuff

;; dispatch functions
(define (query query
               headers
               request-fk
               ;;
               content-type-string
               content-length-string
               body-str)
  (printf "received TRAPI `query` POST request\n")

  (unless (string-contains? (string-downcase content-type-string) "application/json")
    (printf "** unexpected content-type-string for query\nexpected 'application/json', received '~s'\n"
            content-type-string)
    (request-fk))

  (define body-json (string->jsexpr body-str))
  (printf "body-json:\n~s\n" body-json)

  (handle-trapi-query body-json request-fk))


(define (asyncquery query
                    headers
                    request-fk
                    ;;
                    content-type-string
                    content-length-string
                    body-str)
  (printf "received TRAPI `asyncquery` POST request\n")

  (unless (string-contains? (string-downcase content-type-string) "application/json")
    (printf "** unexpected content-type-string for query\nexpected 'application/json', received '~s'\n"
            content-type-string)
    (request-fk))

  (define body-json (string->jsexpr body-str))
  (printf "body-json:\n~s\n" body-json)

  (handle-trapi-asyncquery body-json request-fk))


(define (meta_knowledge_graph query headers request-fk)
  (printf "received TRAPI meta knowledge graph query:\n~s\n" query)
  (list
    'text
    200_OK_STRING
    metaKG.json.txt))

(define (health query headers request-fk)
  (printf "received health status query:\n~s\n" query)
  (list
    'json
    200_OK_STRING
    (string->jsexpr "{}")))

(define (schema.json query headers request-fk)
  (printf "received GET request for schema.json:\n~s\n" query)
  (list
    'text
    200_OK_STRING
    schema.json.txt))

(define (schema.yaml query headers request-fk)
  (printf "received GET request for schema.yaml:\n~s\n" query)
  (list
    'text
    200_OK_STRING
    schema.yaml.txt))


(hash-set! dispatch-table '(GET "schema.json") schema.json)
(hash-set! dispatch-table '(GET "schema.yaml") schema.yaml)

(hash-set! dispatch-table '(POST "query") query)
(hash-set! dispatch-table '(POST "asyncquery") asyncquery)

(hash-set! dispatch-table '(GET "health") health)

(hash-set! dispatch-table '(GET "meta_knowledge_graph") meta_knowledge_graph)

(hash-set!
  dispatch-table
  '(GET "hello")
  (lambda (query headers request-fk)
    (printf "received hello query:\n~s\n" query)
    (list
      'xexpr
      200_OK_STRING
      `(html (body "Hello, World!")))))

(hash-set!
  dispatch-table
  '(GET "syn")
  (lambda (query headers request-fk)
    (printf "received syn query:\n~s\n" query)
    (list
      'xexpr
      200_OK_STRING
      `(html
        (body
         ,(format
            "~s"
            (curie->synonyms-in-db "HGNC:1101")))))))

(hash-set!
  dispatch-table
  '(GET "simple")
  (lambda (query headers request-fk)
    (printf "received simple query:\n~s\n" query)
    (list
      'xexpr
      200_OK_STRING
      `(html
        (body
         ,(format
            "~s"
            (car
              (query:X->Known
                (set->list
                 (get-non-deprecated-mixed-ins-and-descendent-classes*-in-db
                  '("biolink:ChemicalEntity")))
                (set->list
                 (get-non-deprecated-mixed-ins-and-descendent-predicates*-in-db
                  '("biolink:treats")))
                (set->list
                 (get-descendent-curies*-in-db
                  (curie->synonyms-in-db "DOID:9351")))))))))))

(module+ main
  (lognew-info
    (hash 'event "starting_server"))
  (define stop (serve DEFAULT_PORT))
  (lognew-info
    (hash 'event "started_server"))
  (let forever ()
    (sleep 10)
    (forever)))
