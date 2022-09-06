#lang racket/base

(provide serve
         DEFALUT_PORT)
(require
 "../../logging2.rkt"
 ;; "../neo-low-level/query-low-level.rkt"
 ;; "../neo-reasoning/neo-biolink-reasoning.rkt"
 ;; "../neo-utils/neo-helpers.rkt" 
 racket/file
 racket/match
 racket/set
 racket/port
 racket/pretty
 racket/runtime-path
 racket/tcp
 json
 xml
 net/url)

;; Team Unsecret Agent mediKanren 2 server

(define DEFALUT_PORT 8384)

;; Number of seconds before a connection times out, collecting all
;; resources from the connection (was 10 seconds in the original
;; tutorial).
(define CONNECTION_TIMEOUT_SECONDS (* 10 60))

;; per-servelet memory limit (due to garbage collection overhead,
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
  (file->string (path/root "../neo-open-api/TranslatorReasonersAPI.json")))
(define schema.yaml.txt
  (file->string (path/root "../neo-open-api/TranslatorReasonersAPI.yaml")))


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
    (printf "tcp-accept accepted connection\n")
    (thread
     (lambda ()
       (printf "started handle thread\n")
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
       (display (jsexpr->string jsexpr) out)]
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
           
             (printf "Content-Type:\n~s\n" (hash-ref req-headers "Content-Type:" #f))
             (printf "Content-Length:\n~s\n" (hash-ref req-headers "Content-Length:" #f))
         
             (define content-length-string (hash-ref req-headers "Content-Length:" #f))

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
             
             (define content-type-string (hash-ref req-headers "Content-Type:" #f))
             
             (unless content-type-string
               (printf "** error: POST request doesn't include 'Content-Type:'\n")
               (request-fk))
             
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
               (printf "dispatch-result:\n~s\n" dispatch-result)
                     
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
    [else #f])
    
  #f)

(define (make-empty-trapi-response body-json)
  (let* ((message (hash-ref body-json 'message #f))
         (message (hash-set message 'knowledge_graph (hasheq 'nodes (hasheq)
                                                             'edges (hasheq))))
         (message (hash-set message 'results '())))
    (let* ((result (hash-set body-json 'message message)))
      result)))

(define (handle-mvp-creative-query body-json message query_graph edges nodes)
  
  (printf "++ handling MVP mode creative query\n")

  (define trapi-response
    (make-empty-trapi-response body-json))
  
  (list
    'json
    200_OK_STRING
    trapi-response)
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

  (unless (string=? "application/json" content-type-string)
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

  (unless (string=? "application/json" content-type-string)
    (printf "** unexpected content-type-string for query\nexpected 'application/json', received '~s'\n"
            content-type-string)
    (request-fk))

  (define body-json (string->jsexpr body-str))
  (printf "body-json:\n~s\n" body-json)

  (handle-trapi-asyncquery body-json request-fk))


(define (meta_knowledge_graph query headers request-fk)
  (printf "received TRAPI meta knowledge graph query:\n~s\n" query)
  (list
    'json
    200_OK_STRING
    (string->jsexpr "{}")))

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

(hash-set! dispatch-table '(GET "hello") (lambda (query headers request-fk)
                                           (printf "received hello query:\n~s\n" query)
                                           (list
                                             'xexpr
                                             200_OK_STRING
                                             `(html (body "Hello, World!")))))


(module+ main
  (lognew-info
    (hasheq 'event "starting_server"))
  (define stop (serve DEFALUT_PORT))
  (lognew-info
    (hasheq 'event "started_server"))
  (let forever ()
    (sleep 10)
    (forever)))
