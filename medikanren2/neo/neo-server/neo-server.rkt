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
  racket/engine
  racket/set
  racket/port
  racket/pretty
  racket/runtime-path
  racket/string
  racket/tcp
  json
  xml
  net/url
  racket/list)

;; Team Unsecret Agent mediKanren 2 neo server

(define DEFAULT_PORT 8384)

(define NEO_SERVER_VERSION "1.11")

;; Maximum number of results to be returned from *each individual* KP,
;; or from mediKanren itself.
(define MAX_RESULTS_FROM_COMPONENT 250)

;; Maximum number of results to score and then sort.
(define MAX_RESULTS_TO_SCORE_AND_SORT 100000)

;; Number of seconds before a connection times out, collecting all
;; resources from the connection (was 10 seconds in the original
;; tutorial).
(define CONNECTION_TIMEOUT_SECONDS (* 58 60))
(define API_CALL_CONNECTION_TIMEOUT_SECONDS (* 1 60))


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
(define 408_ERROR_STRING "408 Request Timeout")
(define 500_ERROR_STRING "500 Internal Server Error")

(define-runtime-path path:root ".")
(define (path/root relative-path) (build-path path:root relative-path))

(define schema.json.txt
  (file->string (path/root "../neo-open-api/mediKanrenSmartAPI.json")))
(define schema.yaml.txt
  (file->string (path/root "../neo-open-api/mediKanrenSmartAPI.yaml")))
(define metaKG.json.txt
  (file->string (path/root "../neo-open-api/unsecret_metaKG_May_2023.json")))

;; Environment variables (contained in boxes)
(define ENVIRONMENT_TAG_BOX (box #f)) ;; Should be one of "CI", "TEST", or "PROD", if not #f
(define MK_STAGE_BOX (box #f)) ;; deprecated (5 April 2023)
;; (should be removed, once ENVIRONMENT_TAG_BOX is shown to work)

(struct job-failure (message))

(define job-request (make-channel))
(define *jobs-waiting* 0)

(define (work-safely work)
  (printf "entered work-safely\n")
  (let ((result (work)))
    ;; (printf "work-safely returning result ~s\n" result)
    result))

;; Run multiple jobs concurrently
;(define (job work) (work-safely work))

;; Run multiple jobs sequentially
(define (job work)
  (define job-response (make-channel))
  (printf "job is calling channel-put on job-request channel\n")
  (set! *jobs-waiting* (add1 *jobs-waiting*))
  (printf "^ ~s job(s) in job queue\n"
          *jobs-waiting*)
  (channel-put job-request (cons job-response work))
  (printf "job is calling channel-get on job-response channel\n")
  (let ((response (channel-get job-response)))
    (set! *jobs-waiting* (sub1 *jobs-waiting*))
    (printf "work returned response from job-reponse channel\n")
    (printf "^ ~s job(s) remaining in job queue\n"
            *jobs-waiting*)
    response))

(define (serve port-no)
  (define listener
    (tcp-listen port-no
                MAX_ALLOW_WAIT
                REUSE_LISTENER_WHILE_IN_TIME_WAIT_STATE
                ONLY_ACCEPT_CONNECTIONS_FROM_GIVEN_HOSTNAME))
  (define (worker)
    (let loop ()
      (printf "worker is about to call channel-get on job-request\n")
      (match-define (cons job-response work) (channel-get job-request))
      (printf "worker is about to call channel-put on job-response\n")
      (channel-put job-response (work-safely work))
      (loop)))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (thread worker)
  (thread loop))

(define (accept-and-handle listener)
  (define cust.accept-and-handle (make-custodian))
  (parameterize ((current-custodian cust.accept-and-handle))
    (define-values (in out) (tcp-accept listener))
    (printf "\ntcp-accept accepted connection\n")

    ;; main accept-and-handle thread
    (thread
     (lambda ()

       (define start-time-ms-realtime (current-inexact-milliseconds))
       (define timeout-ms-realtime
         (+ start-time-ms-realtime (* CONNECTION_TIMEOUT_SECONDS 1000)))

       (let ((result
              (call/cc
               (lambda (break)
                 (with-handlers
                     ;; Don't send HTTP replies with these
                     ;; unexpected errors, to avoid errors from
                     ;; writing to ports which are closed.
                     ((exn:fail:network:errno?
                       (lambda (ex)
                         (cond
                           ((equal? '(110 . posix) (exn:fail:network:errno-errno ex))
                            (printf "!!! Error: TCP keepalive failed.\n")
                            (printf "    Presuming half-open TCP connection.\n")
                            (break 'tcp-keepalive-failed))
                           (else
                            (printf "!!! Error: Unknown network error ~a.\n" ex)
                            (break 'unknown-network-error)))))
                      (exn:fail?
                       (lambda (v)
                         (printf "!! exn:fail? handler called\n")
                         ((error-display-handler) (exn-message v) v)
                         (break 'exn:fail?-handler-called)))
                      ((lambda _ #t)
                       (lambda (v)
                         (printf "!! Unknown error handler called from job engine\n")
                         (printf "Unknown error: ~s\n" v)
                         (break 'unknown-error))))
                   (let ((result
                          (handle in out
                                  start-time-ms-realtime
                                  timeout-ms-realtime
                                  (lambda ()
                                    (printf "** Connection failure continuation invoked!\n")
                                    (break 'connection-failure))
                                  (lambda ()
                                    (printf "** Request failure continuation invoked!\n")
                                    (break 'request-failure)))))
                     (printf "`handle` call in `engine-proc` finished cleanly\n")
                     result))))))

         (printf "handle returned result ~s\n" result)
       
         (printf "(current-memory-use): ~s\n"
                 (current-memory-use))
         (printf "calling (collect-garbage)\n")
         (collect-garbage)
         (printf "(current-memory-use): ~s\n"
                 (current-memory-use))
       
         (printf "main accept-and-handle thread about to shut-down cust.accept-and-handle\n")
         (custodian-shutdown-all cust.accept-and-handle)
       
         (printf "(current-memory-use): ~s\n"
                 (current-memory-use))
         (printf "calling (collect-garbage)\n")
         (collect-garbage)
         (printf "(current-memory-use): ~s\n"
                 (current-memory-use)))))))

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
               ;(printf "== header:\n~s\n" header)
               ;(printf "== (list-ref header 1):\n~s\n" (list-ref header 1))
               ;(printf "== (list-ref header 2):\n~s\n" (list-ref header 1))
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
(define (handle in out
                start-time-ms-realtime
                timeout-ms-realtime
                conn-fk request-fk)
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
                   (printf "Error: TCP keepalive failed.  Presuming half-open TCP connection.\n")
                   (conn-fk))
                  (else
                   (printf "Error: Unknown network error ~a.\n" ex)
                   (conn-fk))))))
          (match request-type
            ["GET"
             ;; Dispatch GET request:
             (let ([dispatch-result (dispatch-request 'GET
                                                      str-path
                                                      req-headers
                                                      in out
                                                      start-time-ms-realtime
                                                      timeout-ms-realtime
                                                      request-fk)])
               (printf "dispatch-result:\n~s\n" dispatch-result)

               ;; Send reply:
               (send-reply dispatch-result out))]
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
                                                      in out
                                                      start-time-ms-realtime
                                                      timeout-ms-realtime
                                                      request-fk
                                                      ;;
                                                      content-type-string
                                                      content-length-string
                                                      body-str)])
               ;; (printf "dispatch-result:\n~s\n" dispatch-result)
               (printf "about to send reply\n")

               ;; Send reply:
               (send-reply dispatch-result out))]))))))

;; dispatch for HTTP GET and POST requests
(define (dispatch-request request-type
                          str-path
                          req-headers
                          in out
                          start-time-ms-realtime
                          timeout-ms-realtime
                          request-fk
                          . rest-args)

  ;; Parse the request as a URL:
  (define url (string->url str-path))
  ;; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ;; Find a handler based on the path's first element:
  (define dispatch-key (list request-type (car path)))
  (define h/safe? (hash-ref dispatch-table dispatch-key #f))
  (define h (and h/safe? (car h/safe?)))
  (define safe-for-concurrent-use?
    (and h/safe?
         (eq? (cdr h/safe?) 'safe-for-concurrent-use)))
  (printf "dispatch-key: ~s\n" dispatch-key)
  ;(printf "dispatch-table: ~s\n" dispatch-table)
  (printf "h/safe?: ~s\n" h/safe?)
  (printf "h: ~s\n" h)
  (printf "safe-for-concurrent-use?: ~s\n" safe-for-concurrent-use?)
  (printf "url: ~s\n" url)
  (printf "path: ~s\n" path)
  (printf "url-query: ~s\n" (url-query url))
  ;(printf "req-headers: ~s\n" req-headers)
  ;(printf "rest-args: ~s\n" rest-args)
  (newline)

  (cond
    ((not h)
     (printf "!! No handler found !!\n")
     (list
       'xexpr
       `(html (head (title "Error"))
              (body
               (font ((color "red"))
                     "Unknown page: "
                     ,str-path)))))
    (safe-for-concurrent-use?
     (printf "handler is safe for concurrent use\n")
     ;; Call the handler, without using the job channels:
     (apply h (url-query url) req-headers request-fk rest-args))
    (else
     (printf "handler is *not* safe for concurrent use\n")
     ;; Call the handler, using the job channels:
     (define result/cust.job
       (job
        (lambda ()
          (printf "job-thunk invoked\n")

          (printf "(current-memory-use): ~s\n"
                  (current-memory-use))
          (printf "calling (collect-garbage)\n")
          (collect-garbage)
          (printf "(current-memory-use): ~s\n"
                  (current-memory-use))

          (define cust.job (make-custodian))
          (parameterize ((current-custodian cust.job))

            (define ENGINE_GAS_MS (* 1 1000))

            (define eng
              (engine
               (lambda (suspend-proc)
                 ;; apply the request handler
                 (apply h (url-query url) req-headers request-fk rest-args))))

            (with-handlers
                ((exn:fail:network:errno?
                  (lambda (ex)
                    (cond
                      ((equal? '(110 . posix) (exn:fail:network:errno-errno ex))
                       (printf "!!! Error: TCP keepalive failed.\n")
                       (printf "    Presuming half-open TCP connection.\n")
                       (engine-kill eng)
                       (cons
                        (list
                          'xexpr
                          `(html (head (title "Error"))
                                 (body
                                  (font ((color "red"))
                                        "Error: TCP keepalive failed for: "
                                        ,str-path))))
                        cust.job))
                      (else
                       (printf "!!! Error: Unknown network error ~a.\n" ex)
                       (engine-kill eng)
                       (cons
                        (list
                          'xexpr
                          `(html (head (title "Error"))
                                 (body
                                  (font ((color "red"))
                                        "Unknown network error for: "
                                        ,str-path))))
                        cust.job)))
                    ))
                 (exn:fail?
                  (lambda (v)
                    (printf "!! exn:fail? handler called from job engine\n")
                    ((error-display-handler) (exn-message v) v)
                    (engine-kill eng)
                    (cons
                     (list
                       'xexpr
                       `(html (head (title "Error"))
                              (body
                               (font ((color "red"))
                                     "Internal error for: "
                                     ,str-path))))
                     cust.job)))
                 ((lambda _ #t)
                  (lambda (v)
                    (printf "!! Unknown error handler called from job engine\n")
                    (printf "Unknown error: ~s\n" v)
                    (engine-kill eng)
                    (cons
                     (list
                       'xexpr
                       `(html (head (title "Error"))
                              (body
                               (font ((color "red"))
                                     "Internal error for: "
                                     ,str-path))))
                     cust.job))))

              (define unexpected-eof-evt
                (eof-evt in))

              (let loop-forever ()

                (define engine-ran-out-of-gas-evt
                  (alarm-evt (+ (current-inexact-milliseconds)
                                ENGINE_GAS_MS)))
                
                (define end-evt
                  (choice-evt
                   unexpected-eof-evt
                   engine-ran-out-of-gas-evt))

                (let* ((finished? (engine-run end-evt eng))
                       (now-ms-realtime (current-inexact-milliseconds))
                       (elapsed-seconds-realtime
                        (inexact->exact
                         (round
                          (/ (- now-ms-realtime start-time-ms-realtime) 1000.0))))
                       (current-mem (current-memory-use)))
                  (cond
                    (finished?
                     (printf "- Engine ran to completion (~s ~s)\n"
                             elapsed-seconds-realtime
                             current-mem)
                     ;; Make sure to stop the engine.
                     (engine-kill eng)
                     (cons (engine-result eng) cust.job))
                    (else
                     ;; Engine returned because an event in
                     ;; end-evt became ready for
                     ;; synchronization, rather than from the
                     ;; engine's procedure running to
                     ;; completion.  Perform a case analysis on
                     ;; the event that became ready for
                     ;; synchronization.
                     (let ((evt (sync end-evt)))
                       (cond
                         ((or (eof-object? evt)
                              (eq? unexpected-eof-evt evt))
                          (printf
                           "!!! Error: Detected half-closed TCP connection (~s ~s)\n"
                           elapsed-seconds-realtime
                           current-mem)
                          (engine-kill eng)
                          (cons
                           (list
                             'xexpr
                             500_ERROR_STRING
                             `(html
                               (body
                                "Error: TCP keepalive failed.  Presuming half-open TCP connection.")))
                           cust.job))
                         ((eq? engine-ran-out-of-gas-evt evt)
                          (cond
                            ((> now-ms-realtime timeout-ms-realtime)
                             (printf
                              "!!! Computation timed out (~s ~s)\n"
                              elapsed-seconds-realtime
                              current-mem)
                             (engine-kill eng)
                             (cons
                              (list
                                'xexpr
                                408_ERROR_STRING
                                `(html
                                  (body
                                   "Error: Request timeout.")))
                              cust.job))
                            (else
                             (printf ".(~s ~s) "
                                     elapsed-seconds-realtime
                                     current-mem)
                             (flush-output)
                             (loop-forever))))
                         (else
                          (printf "Unexpected value of `(sync end-evt)` (~s ~s): ~s"
                                  elapsed-seconds-realtime
                                  current-mem
                                  evt)
                          (engine-kill eng)
                          (cons
                           (list
                             'xexpr
                             `(html (head (title "Error"))
                                    (body
                                     (font ((color "red"))
                                           "Internal error for: "
                                           ,str-path))))
                           cust.job)))))))))))))
     (define result (car result/cust.job))
     (define cust.job (cdr result/cust.job))
     (printf "about to call (custodian-shutdown-all cust.job)\n")
     (custodian-shutdown-all cust.job)
     (if result
         result
         (begin
           (printf "job returned #f as the result\n")           
           (list
             'xexpr
             `(html (head (title "Error"))
                    (body
                     (font ((color "red"))
                           "Internal error for : "
                           ,str-path)))))))))

(define (mvp-creative-query? edges nodes)

  (define edge* (hash->list edges))
  (printf "edge*: ~s\n" edge*)

  (match edge*
    ;; exactly one edge
    [`((,edge-id . ,edge-hash))
     (define knowledge_type (hash-ref edge-hash 'knowledge_type #f))
     (printf "knowledge_type: ~s\n" knowledge_type)

     (define predicates (hash-ref edge-hash 'predicates #f))
     (printf "predicates: ~s\n" predicates)

     (define subject-id (hash-ref edge-hash 'subject #f))
     (define object-id (hash-ref edge-hash 'object #f))

     (define subject-node (hash-ref nodes (string->symbol subject-id) #f))
     (define object-node (hash-ref nodes (string->symbol object-id) #f))

     (printf "subject-node: ~s\n" subject-node)
     (printf "object-node: ~s\n" object-node)

     (cond
       [(and (equal? "inferred" knowledge_type)
             (list? predicates)
             (member "biolink:treats" predicates)
             subject-node
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
                                      (member "biolink:Disease" object-categories)))))))))
        'mvp1]
       [(and (equal? "inferred" knowledge_type)
             (list? predicates)
             (member "biolink:affects" predicates)
             subject-node
             object-node
             (let ()
               (define object-ids (hash-ref object-node 'ids #f))
               (and (list? object-ids)
                    (not (null? object-ids))
                    (let ()
                      (printf "object-ids: ~s\n" object-ids)
                      (define subject-categories (hash-ref subject-node 'categories #f))
                      (printf "subject-categories: ~s\n" subject-categories)

                      (and (list? subject-categories)
                           (member "biolink:ChemicalEntity" subject-categories)
                           (let ()
                             (define object-categories (hash-ref object-node 'categories #f))
                             (printf "object-categories: ~s\n" object-categories)
                             (and (list? object-categories)
                                  (member "biolink:Gene" object-categories)
                                  (let ()
                                    (define qualifier-constraints (hash-ref edge-hash 'qualifier_constraints #f))
                                    (and qualifier-constraints
                                         (list? qualifier-constraints)
                                         ; only one type of qualifier-constraints and it is the qualifer set
                                         (eq? 1 (length qualifier-constraints))
                                         
                                         (let ()
                                           (define qualifier-set (hash-ref (car qualifier-constraints) 'qualifier_set #f))
                                           (and qualifier-set
                                                (list? qualifier-set)
                                                (eq? 2 (length qualifier-set))
                                                
                                                (let ()
                                                  (define qualifier-a (car qualifier-set))
                                                  (define qualifier-a-type (hash-ref qualifier-a 'qualifier_type_id #f))
                                                  (printf "qualifier-a-type: ~s\n" qualifier-a-type)
                                                  (define qualifier-a-value (hash-ref qualifier-a 'qualifier_value #f))
                                                  (printf "qualifier-a-value: ~s\n" qualifier-a-value)

                                                  (define qualifier-b (cadr qualifier-set))
                                                  (define qualifier-b-type (hash-ref qualifier-b 'qualifier_type_id #f))
                                                  (printf "qualifier-b-type: ~s\n" qualifier-b-type)
                                                  (define qualifier-b-value (hash-ref qualifier-b 'qualifier_value #f))
                                                  (printf "qualifier-b-value: ~s\n" qualifier-b-value)

                                                  (and qualifier-a-type
                                                       qualifier-a-value
                                                       qualifier-b-type
                                                       qualifier-b-value
                                                       (or
                                                        (and (equal? qualifier-a-type "biolink:object_aspect_qualifier")
                                                             (equal? qualifier-b-type "biolink:object_direction_qualifier"))
                                                        (and (equal? qualifier-b-type "biolink:object_aspect_qualifier")
                                                             (equal? qualifier-a-type "biolink:object_direction_qualifier"))))))))))))))))
        'mvp2-gene]
       [(and (equal? "inferred" knowledge_type)
             (list? predicates)
             (member "biolink:affects" predicates)
             subject-node
             object-node
             (let ()
               (define subject-ids (hash-ref subject-node 'ids #f))
               (and (list? subject-ids)
                    (not (null? subject-ids))
                    (let ()
                      (printf "subject-ids: ~s\n" subject-ids)
                      (define subject-categories (hash-ref subject-node 'categories #f))
                      (printf "subject-categories: ~s\n" subject-categories)

                      (and (list? subject-categories)
                           (member "biolink:ChemicalEntity" subject-categories)
                           (let ()
                             (define object-categories (hash-ref object-node 'categories #f))
                             (printf "object-categories: ~s\n" object-categories)

                             (and (list? object-categories)
                                  (member "biolink:Gene" object-categories)
                                  (let ()
                                    (define qualifier-constraints (hash-ref edge-hash 'qualifier_constraints #f))
                                    (and qualifier-constraints
                                         (list? qualifier-constraints)
                                         ; only one type of qualifier-constraints and it is the qualifer set
                                         (eq? 1 (length qualifier-constraints)) 
                                         (let ()
                                           (define qualifier-set (hash-ref (car qualifier-constraints) 'qualifier_set #f))
                                           (and qualifier-set
                                                (list? qualifier-set)
                                                (eq? 2 (length qualifier-set))
                                                
                                                (let ()
                                                  (define qualifier-a (car qualifier-set))
                                                  (define qualifier-a-type (hash-ref qualifier-a 'qualifier_type_id #f))
                                                  (printf "qualifier-a-type: ~s\n" qualifier-a-type)
                                                  (define qualifier-a-value (hash-ref qualifier-a 'qualifier_value #f))
                                                  (printf "qualifier-a-value: ~s\n" qualifier-a-value)

                                                  (define qualifier-b (cadr qualifier-set))
                                                  (define qualifier-b-type (hash-ref qualifier-b 'qualifier_type_id #f))
                                                  (printf "qualifier-b-type: ~s\n" qualifier-b-type)
                                                  (define qualifier-b-value (hash-ref qualifier-b 'qualifier_value #f))
                                                  (printf "qualifier-b-value: ~s\n" qualifier-b-value)

                                                  (and qualifier-a-type
                                                       qualifier-a-value
                                                       qualifier-b-type
                                                       qualifier-b-value
                                                       (or
                                                        (and (equal? qualifier-a-type "biolink:object_aspect_qualifier")
                                                             (equal? qualifier-b-type "biolink:object_direction_qualifier"))
                                                        (and (equal? qualifier-b-type "biolink:object_aspect_qualifier")
                                                             (equal? qualifier-a-type "biolink:object_direction_qualifier"))))))))))))))))
        'mvp2-chem]
       [else #f])]
             
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
      (map (lambda (h i) (set-score-in-result h (score-one-result h i))) results (iota n)))))

(define auxiliary-graph-attribute
  (lambda (id)
    (hash
     'attribute_type_id "biolink:support_graphs"
     'value (list id))))

(define edge-has-source?
  (lambda (props)
    (and 
     (or (get-assoc "biolink:primary_knowledge_source" props)
         (get-assoc "knowledge_source" props)
         (and (get-assoc "json_attributes" props)
              (let ((attr-hl (string->jsexpr (get-assoc "json_attributes" props))))
                (let loop ((hl attr-hl))
                  (cond
                    ((null? hl) #f)
                    ((equal?
                      (hash-ref (car hl) 'attribute_type_id #f)
                      "biolink:primary_knowledge_source")
                     #t)
                    (else (loop (cdr hl))))))))
     (or (get-assoc "publications" props)
         (get-assoc "supporting_publications" props)
         (and (get-assoc "json_attributes" props)
              (let ((attr-hl (string->jsexpr (get-assoc "json_attributes" props))))
                (let loop ((hl attr-hl))
                  (cond
                    ((null? hl) #f)
                    ((equal?
                      (hash-ref (car hl) 'attribute_type_id #f)
                      "biolink:publications")
                     #t)
                    (else (loop (cdr hl)))))))))))

(define (data-attributes props)
  (let ((publication (get-assoc "publications" props)))
    (list (hash
           'attribute_type_id "biolink:publications"
           'value (if (string-prefix? publication "(")
                      (string-split (string-trim (string-trim publication "(") ")"))
                      (string-split publication "|"))
           'value_type_id "biolink:Uriorcurie"))))

(define unsecret-source
  (hash
      'resource_id "infores:unsecret-agent"
      'resource_role "aggregator_knowledge_source"))

(define (get-source props)
  (let ((source (or (get-assoc "biolink:primary_knowledge_source" props)
                    (get-assoc "knowledge_source" props) ;rkx-kg2pre2.8.0
                    (and (get-assoc "json_attributes" props)
                         "infores:text-mining-provider-targeted")))) ;text-mining
    (hash
      'resource_id source
      'resource_role "primary_knowledge_source")))

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
  (let ((pubs (or (get-assoc "publications" props)
                  (get-assoc "supporting_publications" props))))
    (if pubs
        (max (length (string-split pubs "|")) (length (string-split pubs)))
        0)))

(define (get-score-from-result result)
  (let ((analyses (hash-ref result 'analyses #f)))
    (if analyses
        (hash-ref (car analyses) 'score)
        (error "check the implementation of results.analyses"))))

(define (set-score-in-result result score)
  (let ((analyses (hash-ref result 'analyses #f)))
    (if analyses
        (hash-set result 'analyses
                  (list (hash-set (car analyses) 'score score)))
        (error "check the implementation of results.analyses"))))

(define (normalize-scores results)
  (if (null? results)
      results
      (let ((max-score (get-score-from-result (car results))))
        (map (lambda (x) (set-score-in-result x (/ (get-score-from-result x) (* 1.0 max-score)))) results))))

(define (merge-list xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (merge-list ys (cdr xs)))))

(define (merge-hash h1 h2)
  (define h h2)
  (hash-for-each h1 (lambda (k v) (set! h (hash-set h k v))))
  h)

;; TODO: test
(define (merge-trapi-responses r1 r2 original-query_graph)
  (let* ((message1 (hash-ref r1 'message))
         (message2 (hash-ref r2 'message))
         (auxiliary_graphs1 (hash-ref message1 'auxiliary_graphs))
         (auxiliary_graphs2 (hash-ref message2 'auxiliary_graphs))
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
           'auxiliary_graphs
           (merge-hash auxiliary_graphs1 auxiliary_graphs2)
           ;;
           'results
           (merge-list results1 results2)
           ))))

(define (handle-mvp-creative-query body-json message query_graph edges nodes which-mvp)

  (printf "++ handling MVP mode creative query for Neo Server ~a\n" NEO_SERVER_VERSION)

  (define disable-external-requests
    (hash-ref message 'disable_external_requests #f))

  (define our-trapi-response
    (let ()

      (define query_graph (hash-ref message 'query_graph))
      (define qg_nodes (hash-ref query_graph 'nodes))
      (define qg_edges (hash-ref query_graph 'edges))
      (define qg_edge* (hash->list qg_edges))

      (match-define (list qg_edge-id qg_edge-hash)
        (match qg_edge*
          ;; exactly one edge
          [`((,edge-id . ,edge-hash))
           (list edge-id edge-hash)]))

      (define qg_object-node-str (hash-ref qg_edge-hash 'object))
      (define qg_object-node-id (string->symbol qg_object-node-str))

      (define qg_subject-node-str (hash-ref qg_edge-hash 'subject))
      (define qg_subject-node-id (string->symbol qg_subject-node-str))

      ; TODO: Would new mvp support more than one predicates?
      (define qg_predicate-str (car (hash-ref qg_edge-hash 'predicates)))

      (define mvp2-filter
        (lambda (q direction)
          (filter
           (lambda (e)
             (let-values ([(_ eprop) (split-at e 5)])
               (let* ((target-eprop (cadr eprop))
                      (aspect-pr (assoc "object_aspect_qualifier" target-eprop))
                      (direction-pr (assoc "object_direction_qualifier" target-eprop)))
                 (and
                  aspect-pr
                  direction-pr
                  (or
                   (equal? "activity" (cadr aspect-pr))
                   (equal? "abundance" (cadr aspect-pr))
                   (equal? "activity_or_abundance" (cadr aspect-pr)))
                  (equal? direction (cadr direction-pr))
                  ))))
           q)))

      (match-define (list input-id* q1-all-results-unsorted)
        (time
         (cond
           [(eq? 'mvp1 which-mvp)
            (define disease-ids
              ;; TODO write a chainer in utils, and also check for errors
              (hash-ref (hash-ref qg_nodes qg_object-node-id) 'ids))
            ;;
            (let ((q
                   ;; TODO
                   ;;
                   ;; * ensure all of the biolink curies are supported in
                   ;; the current biolink standard, or replace
                   ;;
                   ;; * use qualified predicates
                   (query:X->Y->Known
                    ;; X
                    (set->list
                     (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
                      '("biolink:ChemicalEntity")))
                    (set->list
                     (get-non-deprecated/mixin/absreact-ins-and-descendent-predicates*-in-db
                      '("biolink:affects")))
                    ;; Y
                    (set->list
                     (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
                      '("biolink:Gene" "biolink:GeneOrGeneProduct" "biolink:Protein")))
                    (set->list
                     (get-non-deprecated/mixin/absreact-ins-and-descendent-predicates*-in-db
                      '("biolink:gene_associated_with_condition"
                        "biolink:contributes_to")))
                    ;;
                    (set->list
                     (get-descendent-curies*-in-db
                      (curies->synonyms-in-db disease-ids))))))
               (list disease-ids q))]
           [(eq? 'mvp2-chem which-mvp)
            (define chemical-ids
              (hash-ref (hash-ref qg_nodes qg_subject-node-id) 'ids))
            (define direction
              (let ((qualifer-set
                     (hash-ref (car (hash-ref qg_edge-hash 'qualifier_constraints)) 'qualifier_set)))
                (let loop ((l qualifer-set))
                  (if (equal? (hash-ref (car l) 'qualifier_type_id) "biolink:object_direction_qualifier")
                      (hash-ref (car l) 'qualifier_value)
                      (loop (cdr l))))))
            (let* ((q
                    (query:Known->Y->X
                     (set->list
                      (get-descendent-curies*-in-db
                       (curies->synonyms-in-db chemical-ids)))
                     '("biolink:affects" "biolink:regulates")
                     #f
                     '("biolink:affects")
                     (set->list
                      (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
                       '("biolink:Gene" "biolink:Protein")))))
                   (qualified-q (mvp2-filter q direction)))
              (list chemical-ids qualified-q))]
           [(eq? 'mvp2-gene which-mvp)
            (define gene-ids
              (hash-ref (hash-ref qg_nodes qg_object-node-id) 'ids))
            (define direction
              (let ((qualifer-set
                     (hash-ref (car (hash-ref qg_edge-hash 'qualifier_constraints)) 'qualifier_set)))
                (let loop ((l qualifer-set))
                  (if (equal? (hash-ref (car l) 'qualifier_type_id) "biolink:object_direction_qualifier")
                      (hash-ref (car l) 'qualifier_value)
                      (loop (cdr l))))))
            (let* ((q
                    (query:X->Y->Known
                     (set->list
                      (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
                       '("biolink:ChemicalEntity")))
                     '("biolink:affects" "biolink:regulates")
                     #f
                     '("biolink:affects")
                     (set->list
                      (get-descendent-curies*-in-db
                       (curies->synonyms-in-db gene-ids)))))
                   (qualified-q (mvp2-filter q direction)))
              (list gene-ids qualified-q))])))

      (printf "computed a total of ~s results for MVP mode creative query\n"
              (length q1-all-results-unsorted))

      (define q1-unsorted-long (take-at-most q1-all-results-unsorted MAX_RESULTS_TO_SCORE_AND_SORT))

      (printf "about to score ~s results for MVP mode creative query\n"
              (length q1-unsorted-long))

      (define (score-mvp-two-hop-edge e)
        (match e
          [`(,curie_x
             ,pred_xy
             ,curie_y
             ,pred_yz
             ,curie_z
             ,props_xy
             ,props_yz)
           (if (and (edge-has-source? props_xy)
                    (edge-has-source? props_yz))
               (* (num-pubs props_xy) (num-pubs props_yz))
               -1000000)]))

      (define scored/q1-unsorted-long
        (map
         (lambda (e) (cons (score-mvp-two-hop-edge e) e))
         q1-unsorted-long))

      (printf "about to sort ~s results for MVP mode creative query\n"
              (length scored/q1-unsorted-long))

      (define scored/q1-sorted-long
        (sort
         scored/q1-unsorted-long
         (lambda (score1/e1 score2/e2)
           (let ((score1 (car score1/e1))
                 (score2 (car score2/e2)))
             (> score1 score2)))))

      (printf "about to take the first ~s scored and sorted results for MVP mode creative query\n"
              MAX_RESULTS_FROM_COMPONENT)

      (define scored/q1-sorted (take-at-most scored/q1-sorted-long MAX_RESULTS_FROM_COMPONENT))

      (printf "now have ~s scored and sorted results for MVP mode creative query\n"
              (length scored/q1-sorted))

      (define nodes (make-hash))

      (define edges (make-hash))

      (define auxiliary-graph (make-hash))

      (define unmerged-results (make-hash))

      (define (add-node! curie)
        (let ((props (curie->properties curie)))
          (let ((categories (list-assoc "category" props))
                (name (get-assoc "name" props)))
            (hash-set! nodes (string->symbol curie)
                       (hash 'categories (filter
                                          (lambda (c)
                                            (not (or
                                                  (class-mixin? c)
                                                  (class-abstract? c)))) categories)
                             'name name)))))
      
      (define (add-edge! props n)             
        (let ((id (string-append "medik:edge#" (number->string n))))
          (hash-set! edges (string->symbol id)
                     (hash 'attributes
                            (or
                             (and (get-assoc "json_attributes" props)
                                  (string->jsexpr (get-assoc "json_attributes" props)))
                             (data-attributes props))
                           'object (get-assoc "object" props)
                           'predicate (get-assoc "predicate" props)
                           'subject (get-assoc "subject" props)
                           'sources (list (get-source props) unsecret-source)))
          id))

      (define (add-creative-edge! sub obj pred n aux-id e1prop e2prop)
        (let ((id (string-append "medik:creative_edge#" (number->string n))))
          (hash-set! edges (string->symbol id)
                     (hash 'attributes
                           (list*
                            (auxiliary-graph-attribute aux-id)
                            (append 
                             (or
                              (and (get-assoc "json_attributes" e1prop)
                                   (string->jsexpr (get-assoc "json_attributes" e1prop)))
                              (data-attributes e1prop))
                             (or
                              (and (get-assoc "json_attributes" e2prop)
                                   (string->jsexpr (get-assoc "json_attributes" e2prop)))
                              (data-attributes e2prop))))
                           'object obj
                           'predicate pred
                           'subject sub
                           'sources (list (get-source e1prop)
                                          (get-source e2prop)
                                          unsecret-source)))
          id))

      (define (add-auxiliary! edge* n)
        (let ((id (string-append "medik:auxiliary_graph#" (number->string n))))
          (hash-set! auxiliary-graph (string->symbol id)
                     (hash 'edges edge*))
          id))

      (define (add-unmerged-result! r)
        (hash-update! unmerged-results (hash-ref r 'result_id)
                           (lambda (r-old)
                             (let* ((a*-old (hash-ref r-old 'analyses))
                                    (a-old (car a*-old))
                                    (edge-old (hash-ref (hash-ref a-old 'edge_bindings) qg_edge-id))
                                    (score-old (hash-ref a-old 'score))
                                    (a*-new (hash-ref r 'analyses))
                                    (a-newo (car a*-new))
                                    (edge-new (hash-ref  (hash-ref a-newo 'edge_bindings) qg_edge-id))
                                    (score-new (hash-ref a-newo 'score)))
                               (hash
                                'node_bindings (hash-ref r-old 'node_bindings) 
                                'analyses (list (hash 'edge_bindings (hash qg_edge-id (remove-duplicates (append edge-old edge-new)))
                                                      'resource_id "infores:unsecret-agent"
                                                      'score (max score-old score-new))))))
                           r))

      (let loop ((en 0) (an 0) (score*/e* scored/q1-sorted))
        (cond
          ((null? score*/e*) '())
          (else
           (define score/e (car score*/e*))
           (define score (car score/e))
           (define e (cdr score/e))
           (match e
             [`(,curie_x
                ,pred_xy
                ,curie_y
                ,pred_yz
                ,curie_z
                ,props_xy
                ,props_yz)
              (if (and (edge-has-source? props_xy)
                       (edge-has-source? props_yz))
                  (begin 
                    (add-node! curie_x)
                    (add-node! curie_y)
                    (add-node! curie_z)
                    (let* ((edge_xy (add-edge! props_xy en))
                           (edge_yz (add-edge! props_yz (+ en 1)))
                           (auxiliary_id (add-auxiliary! (list edge_xy edge_yz) an))
                           (edge_creative (add-creative-edge! curie_x
                                                              curie_z
                                                              qg_predicate-str
                                                              an
                                                              auxiliary_id
                                                              props_xy
                                                              props_yz)))
                      (add-unmerged-result!
                             (cond
                               [(or (eq? which-mvp 'mvp1) (eq? which-mvp 'mvp2-gene))
                                (hash 'node_bindings
                                      (if (equal? curie_z (car input-id*))
                                          (hash
                                           qg_subject-node-id (list (hash 'id curie_x))
                                           qg_object-node-id (list (hash 'id curie_z)))
                                          (hash
                                           qg_subject-node-id (list (hash 'id curie_x))
                                           qg_object-node-id (list (hash 'id curie_z
                                                                         'query_id (car input-id*)))))
                                      'result_id curie_x
                                      'analyses
                                      (list (hash
                                             'resource_id "infores:unsecret-agent"
                                             'edge_bindings
                                             (hash qg_edge-id (list (hash 'id edge_creative)))
                                             'score score)))]
                               [(eq? which-mvp 'mvp2-chem)
                                (hash 'node_bindings
                                      (if (equal? curie_x (car input-id*))
                                          (hash
                                           qg_subject-node-id (list (hash 'id curie_x))
                                           qg_object-node-id (list (hash 'id curie_z)))
                                          (hash
                                           qg_subject-node-id (list (hash 'id curie_x
                                                                          'query_id (car input-id*)))
                                           qg_object-node-id (list (hash 'id curie_z))))
                                      'result_id curie_z
                                      'analyses
                                      (list (hash
                                             'resource_id "infores:unsecret-agent"
                                             'edge_bindings
                                             (hash qg_edge-id (list (hash 'id edge_creative)))
                                             'score score)))])))
                    (loop (+ en 2) (+ an 1) (cdr score*/e*)))
                  (loop en an (cdr score*/e*)))]))))

      (define merged-results 
        (let loop ((id* (hash-keys unmerged-results))
                   (r '()))
          (cond
            [(null? id*) r]
            [else (loop (cdr id*)
                        (cons (hash-ref unmerged-results (car id*)) r))])))
      
      ;; TODO: the result should be already sorted from the process above
      (define results (sort merged-results (lambda (a b) (> (get-score-from-result a) (get-score-from-result b)))))

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
        'auxiliary_graphs
        auxiliary-graph
        ;;
        'results
        (normalize-scores results)))
      ))

  (define gp-trapi-response
    ;; Disable Genetics Provider TRAPI calls for now, until Genetics
    ;; Provider KP is handling TRAPI 1.4 Creative Mode queries.
    #f
    #;(if disable-external-requests
        #f
        (let ()

          (define res #f)

          (printf "making sync/timeout API call with timeout of ~s seconds\n"
                  API_CALL_CONNECTION_TIMEOUT_SECONDS)

          (sync/timeout
           API_CALL_CONNECTION_TIMEOUT_SECONDS
           (thread
            (lambda ()
              ;; use 'url.genetics.prod', 'url.genetics.test', or 'url.genetics.ci'
              ;; based on the environment the Unsecret server is running in.
              (let ((kp-url (case (unbox ENVIRONMENT_TAG_BOX)
                              (("CI") url.genetics.ci)
                              (("TEST") url.genetics.test)
                              (("PROD") url.genetics.prod)
                              (else
                               (lognew-info
                                (hash 'event
                                      (format
                                       "unexpected ENVIRONMENT_TAG_BOX value: '~s'"
                                       (unbox ENVIRONMENT_TAG_BOX))))
                               url.genetics.prod))))
                (lognew-info
                 (hash 'event (format "kp-url for Genetics Provider call: '~s'" kp-url)))
                (set! res
                      (api-query (string-append kp-url path.query)
                                 body-json))))))

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
                                            'sources
                                            (cons unsecret-source
                                                  (hash-ref v 'sources)))))))

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

  (define versioned-trapi-response
    (hash-set* scored-trapi-response
               'schema_version "1.4.0"
               'biolink_version "3.1.2"))
  
  (list
   'json
   200_OK_STRING
   versioned-trapi-response)
  )


(define (handle-trapi-query body-json request-fk)

  (define message (hash-ref body-json 'message #f))
  ;(printf "message:\n~s\n" message)
  (unless message
    (printf "** missing `message` in `body-json`: ~s\n" body-json)
    (request-fk))

  (define query_graph (hash-ref message 'query_graph #f))
  ;(printf "query_graph:\n~s\n" query_graph)
  (unless query_graph
    (printf "** missing `query_graph` in `message`: ~s\n" message)
    (request-fk))

  (define edges (hash-ref query_graph 'edges #f))
  ;(printf "edges:\n~s\n" edges)
  (unless edges
    (printf "** missing `edges` in `query_graph`: ~s\n" query_graph)
    (request-fk))

  (define nodes (hash-ref query_graph 'nodes #f))
  ;(printf "nodes:\n~s\n" nodes)
  (unless nodes
    (printf "** missing `nodes` in `query_graph`: ~s\n" query_graph)
    (request-fk))

  (define creative-mvp? (mvp-creative-query? edges nodes))
  (printf "creative-mvp?: ~s\n" creative-mvp?)

  (if creative-mvp?
      (handle-mvp-creative-query body-json message query_graph edges nodes creative-mvp?)
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
      (handle-mvp-creative-query body-json message query_graph edges nodes creative-mvp?)
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
  ;(printf "body-json:\n~s\n" body-json)

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


;; Dispatch table request handlers that are safe for concurrent use,
;; without synchronization through the job channels:
(hash-set! dispatch-table '(GET "schema.json") (cons schema.json 'safe-for-concurrent-use))
(hash-set! dispatch-table '(GET "schema.yaml") (cons schema.yaml 'safe-for-concurrent-use))

(hash-set! dispatch-table '(GET "meta_knowledge_graph")
           (cons meta_knowledge_graph 'safe-for-concurrent-use))

(hash-set! dispatch-table '(GET "health") (cons health 'safe-for-concurrent-use))

(hash-set!
 dispatch-table
 '(GET "hello")
 (cons
   (lambda (query headers request-fk)
     (printf "received hello query:\n~s\n" query)
     (list
       'xexpr
       200_OK_STRING
       `(html (body ,(format "Hello, World! from Neo Server ~a" NEO_SERVER_VERSION)))))
   'safe-for-concurrent-use))


;; Dispatch table request handlers that are *not* safe for concurrent
;; use, and whose use must be synchronized through the job channels:
(hash-set! dispatch-table '(POST "query") (cons query #f))
(hash-set! dispatch-table '(POST "asyncquery") (cons asyncquery #f))

(hash-set!
 dispatch-table
 '(GET "syn")
 (cons
   (lambda (query headers request-fk)
     (printf "received syn query:\n~s\n" query)
     (list
       'xexpr
       200_OK_STRING
       `(html
         (body
          ,(format
            "~s"
            (curie->synonyms-in-db "HGNC:1101"))))))
   #f))

(hash-set!
 dispatch-table
 '(GET "simple")
 (cons
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
               (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
                '("biolink:ChemicalEntity")))
              (set->list
               (get-non-deprecated/mixin/absreact-ins-and-descendent-predicates*-in-db
                '("biolink:treats")))
              (set->list
               (get-descendent-curies*-in-db
                (curie->synonyms-in-db "DOID:9351"))))))))))
   #f))

(module+ main
  (lognew-info
   (hash 'event "About to check server environment variables"))
  (set-box! ENVIRONMENT_TAG_BOX (getenv "ENVIRONMENT_TAG"))
  (lognew-info
   (hash 'event (format "ENVIRONMENT_TAG_BOX value = '~s'" (unbox ENVIRONMENT_TAG_BOX))))
  (set-box! MK_STAGE_BOX (getenv "MK_STAGE"))
  (lognew-info
   (hash 'event (format "MK_STAGE_BOX value = '~s'" (unbox MK_STAGE_BOX))))  
  (lognew-info
   (hash 'event "starting_server"))
  (lognew-info
   (hash 'event (format "(Neo Server ~a)" NEO_SERVER_VERSION)))  
  (serve DEFAULT_PORT)
  (lognew-info
   (hash 'event "started_server"))
  (let forever ()
    (sleep 10)
    (forever)))



;;; Ensure the data is loaded by running an example query:
(define q3 (query:X->Known
            (set->list
             (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
              '("biolink:ChemicalEntity")))
            (set->list
             (get-non-deprecated/mixin/absreact-ins-and-descendent-predicates*-in-db
              '("biolink:treats")))
            (set->list
             (get-descendent-curies*-in-db
              (curie->synonyms-in-db "DOID:9351")))))

(length q3)
