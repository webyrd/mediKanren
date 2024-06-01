#lang racket/base

(provide serve
         DEFAULT_PORT)
(require
  "../../logging2.rkt"
  "../neo-low-level/query-low-level-multi-db.rkt"
  "../neo-open-api/neo-api-query.rkt"
  "../neo-reasoning/neo-biolink-reasoning.rkt"
  "../neo-utils/neo-helpers-multi-db.rkt"
  "neo-server-utils.rkt"
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
  racket/list
  racket/math
  )

;; Team Unsecret Agent mediKanren 2 neo server

(define DEFAULT_PORT 8384)

(define NEO_SERVER_VERSION "1.45")

;; Maximum number of results to be returned from *each individual* KP,
;; or from mediKanren itself.
(define MAX_RESULTS_FROM_COMPONENT 250)

;; Maximum number of results to score and then sort.
(define MAX_RESULTS_TO_SCORE_AND_SORT 100000)

(define MAX_DESCENDENT 100)

;; Number of seconds before a connection times out, collecting all
;; resources from the connection (was 10 seconds in the original
;; tutorial).
(define CONNECTION_TIMEOUT_SECONDS (* 58 60))
(define API_CALL_CONNECTION_TIMEOUT_SECONDS (* 1 60))

;; Numbers of the top bucket of the RoboKop KG, Text Mining KG, and RTX-KG2 KG.
(define TOP_BUCKET_NUMBERS (list (list (get-highest-bucket-number-robokop))
                                 (list (get-highest-bucket-number-text-mining))
                                 (list (get-highest-bucket-number-rtx-kg2))))

;; Unsecret-level excluded MVP1 results - answers that is obviously wrong/useless
(define UNWELCOME-TREATMENT
  (curies->synonyms
   '(
     "MESH:D001335" ; Vehicle emissions
     "UMLS:C0013227" ; Pharmaceutical preparations
     "MESH:D014028" ; tobacco smoke pollution
     "UMLS:C1611640" ; Therapeutic agent (substance)
     )))


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
  (file->string (path/root "../neo-open-api/mediKanrenSmartAPI_1_4.json")))
(define schema.yaml.txt
  (file->string (path/root "../neo-open-api/mediKanrenSmartAPI_1_4.yaml")))
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
       (display (jsexpr->string jsexpr) out)
       #;(pretty-print-json-string (jsexpr->string jsexpr) out)]
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
                                                (eq? 3 (length qualifier-set))
                                                (let ()
                                                  (match-define
                                                    (list qualifier-a-type qualifier-a-value)
                                                    (get-and-print-qualifiers (list-ref qualifier-set 0)))
                                                  (match-define
                                                    (list qualifier-b-type qualifier-b-value)
                                                    (get-and-print-qualifiers (list-ref qualifier-set 1)))
                                                  (match-define
                                                    (list qualifier-c-type qualifier-c-value)
                                                    (get-and-print-qualifiers (list-ref qualifier-set 2)))
                                                  (define expected-qualifiers (list "biolink:object_aspect_qualifier"
                                                                                    "biolink:object_direction_qualifier"
                                                                                    "biolink:qualified_predicate"))
                                                  (and qualifier-a-type
                                                       qualifier-a-value
                                                       qualifier-b-type
                                                       qualifier-b-value
                                                       qualifier-c-type
                                                       qualifier-c-value
                                                       (member qualifier-a-type expected-qualifiers)
                                                       (member qualifier-b-type expected-qualifiers)
                                                       (member qualifier-c-type expected-qualifiers))))))))))))))
        'mvp2-gene]
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
                                                  (match-define
                                                    (list qualifier-a-type qualifier-a-value)
                                                    (get-and-print-qualifiers (list-ref qualifier-set 0)))
                                                  (match-define
                                                    (list qualifier-b-type qualifier-b-value)
                                                    (get-and-print-qualifiers (list-ref qualifier-set 1)))
                                                  (define expected-qualifiers (list "biolink:object_aspect_qualifier"
                                                                                    "biolink:object_direction_qualifier"))
                                                  (and qualifier-a-type
                                                       qualifier-a-value
                                                       qualifier-b-type
                                                       qualifier-b-value
                                                       (member qualifier-a-type expected-qualifiers)
                                                       (member qualifier-b-type expected-qualifiers))))))))))))))
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
                                                (eq? 3 (length qualifier-set))
                                                (let ()
                                                  (match-define
                                                    (list qualifier-a-type qualifier-a-value)
                                                    (get-and-print-qualifiers (list-ref qualifier-set 0)))
                                                  (match-define
                                                    (list qualifier-b-type qualifier-b-value)
                                                    (get-and-print-qualifiers (list-ref qualifier-set 1)))
                                                  (match-define
                                                    (list qualifier-c-type qualifier-c-value)
                                                    (get-and-print-qualifiers (list-ref qualifier-set 2)))
                                                  (define expected-qualifiers (list "biolink:object_aspect_qualifier"
                                                                                    "biolink:object_direction_qualifier"
                                                                                    "biolink:qualified_predicate"))       
                                                  (and qualifier-a-type
                                                       qualifier-a-value
                                                       qualifier-b-type
                                                       qualifier-b-value
                                                       qualifier-c-type
                                                       qualifier-c-value
                                                       (member qualifier-a-type expected-qualifiers)
                                                       (member qualifier-b-type expected-qualifiers)
                                                       (member qualifier-c-type expected-qualifiers))))))))))))))
        'mvp2-chem]
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
                                                  (match-define
                                                    (list qualifier-a-type qualifier-a-value)
                                                    (get-and-print-qualifiers (list-ref qualifier-set 0)))
                                                  (match-define
                                                    (list qualifier-b-type qualifier-b-value)
                                                    (get-and-print-qualifiers (list-ref qualifier-set 1)))
                                                  (define expected-qualifiers (list "biolink:object_aspect_qualifier"
                                                                                    "biolink:object_direction_qualifier"))
                                                  (and qualifier-a-type
                                                       qualifier-a-value
                                                       qualifier-b-type
                                                       qualifier-b-value
                                                       (member qualifier-a-type expected-qualifiers)
                                                       (member qualifier-b-type expected-qualifiers))))))))))))))
        'mvp2-chem]
       [else #f])]

    [else #f]))

(define (make-empty-trapi-response)
    (hash
     'message
     (hash
      ;;
      'auxiliary_graphs
      (hash)
      ;;
      'query_graph
      (hash)
      ;;
      'knowledge_graph
      (hash 'nodes (hash)
            'edges (hash))
      ;;
      ;;'results
      ;;'()
      )))

(define (make-score-result num-results)
  (lambda (result index)
    (- num-results index)))

(define (score-results results)
  (let ((n (length results)))
    (let ((score-one-result (make-score-result n)))
      (map (lambda (h i) (set-score-in-result h (score-one-result h i))) results (iota n)))))

(define UNSECRET-SOURCE
  (hash
      'resource_id "infores:unsecret-agent"
      'resource_role "aggregator_knowledge_source"))

(define (excluded-semmed-edge? prop)
  (let ((source (get-assoc "primary_knowledge_source" prop))) ;only rkx-kg2 has semmed edge
    (and (equal? source "infores:semmeddb")
         (let* ((sub (get-assoc "subject" prop))
                (sub-cat (car (list-assoc "category" (curie->properties sub))))
                (obj (get-assoc "object" prop))
                (obj-cat (car (list-assoc "category" (curie->properties obj))))
                (pred (get-assoc "predicate" prop))
                (semantic-pat-obj (list #f #f obj-cat))
                (semantic-pat-sub (list sub-cat #f #f))
                (domain-pat (list sub-cat pred #f))
                (range-pat (list #f pred obj-cat)))
           (or (member semantic-pat-obj semantic-exclude*)
               (member semantic-pat-sub semantic-exclude*)
               (member domain-pat domain-exclude*)
               (member range-pat range-exclude*))))))

(define (not-semmed-excluded? e)
  (match e
    [`(,curie_x
       ,pred_xy
       ,curie_y
       ,(? string? pred_yz)
       ,(? string? curie_z)
       ,props_xy
       ,props_yz)
     (and (not (excluded-semmed-edge? props_xy))
          (not (excluded-semmed-edge? props_yz)))]
    [`(,curie_x
       ,pred_xy
       ,curie_y
       .
       ,props_xy)
     (not (excluded-semmed-edge? props_xy))]))

(define (not-unwelcome-treatment? e)
  (not (member (car e) UNWELCOME-TREATMENT)))


(define (node-has-name-and-cat? curie)
  (let* ((props (curie->properties curie))
         (categories (list-assoc "category" props))
         (categories (filter
                      (lambda (c)
                        (not (or
                              (class-mixin? c)
                              (class-abstract? c))))
                      categories))
         (name (get-assoc "name" props)))
    (and (not (null? categories)) name)))

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

      (match-define (list input-id* q-1hop-results q-2hop-results)
        (time
         (cond
           [(eq? 'mvp1 which-mvp)
            ;;
            (define disease-ids
              ;; TODO write a chainer in utils, and also check for errors
              (hash-ref (hash-ref qg_nodes qg_object-node-id) 'ids))
            (define disease-ids+
              (set->list
               (get-n-descendent-curies*-in-db
                (curies->synonyms-in-db disease-ids)
                MAX_DESCENDENT)))
            (define chemical-catogory+
              (set->list
               (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
                '("biolink:ChemicalEntity"))))
            ;;
            (define 1-hop-proc
              (lambda (score*)
                (filter (lambda (r)
                          (and (not-semmed-excluded? r)
                               (not-unwelcome-treatment? r)))
                        (query:X->Known-scored
                         chemical-catogory+
                         '("biolink:treats"
                           "biolink:treats_or_applied_or_studied_to_treat")
                         disease-ids+
                         score*))))
            ;;
            (let ((q-1hop (auto-grow 1-hop-proc TOP_BUCKET_NUMBERS MAX_RESULTS_FROM_COMPONENT))
                  (q-2hop (query:X->Y->Known-auto-grow
                           chemical-catogory+
                           '("biolink:affects" "biolink:regulates")
                           (set->list
                            (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
                             '("biolink:Gene" "biolink:GeneOrGeneProduct" "biolink:Protein")))
                           (set->list
                            (get-non-deprecated/mixin/abstract-ins-and-descendent-predicates*-in-db
                             '("biolink:gene_associated_with_condition"
                               "biolink:contributes_to")))
                           disease-ids+
                           TOP_BUCKET_NUMBERS
                           MAX_RESULTS_FROM_COMPONENT
                           (lambda (r*) (filter (lambda (r)
                                                  (and (not-semmed-excluded? r)
                                                       (not-unwelcome-treatment? r)))
                                                r*)))))
              (list disease-ids q-1hop q-2hop))]
           [(eq? 'mvp2-chem which-mvp)
            ;;
            (define chemical-ids
              (hash-ref (hash-ref qg_nodes qg_subject-node-id) 'ids))
            (define chemical-ids+
              (time (set->list
                     (get-n-descendent-curies*-in-db
                      (curies->synonyms-in-db chemical-ids)
                      MAX_DESCENDENT))))
            (define direction
              (let ((qualifer-set
                     (hash-ref (car (hash-ref qg_edge-hash 'qualifier_constraints)) 'qualifier_set)))
                (let loop ((l qualifer-set))
                  (if (equal? (hash-ref (car l) 'qualifier_type_id) "biolink:object_direction_qualifier")
                      (hash-ref (car l) 'qualifier_value)
                      (loop (cdr l))))))
            (define gene-category+
              (set->list
               (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
                '("biolink:Gene" "biolink:Protein"))))
            ;;
            (define 1-hop-proc
              (lambda (score*)
                (filter not-semmed-excluded?
                        (mvp2-1hop-filter
                         (query:Known->X-scored
                          chemical-ids+
                          '("biolink:affects")
                          gene-category+
                          score*)
                         direction))))
            ;;
            (let* ((qualified-q-1hop (auto-grow 1-hop-proc TOP_BUCKET_NUMBERS MAX_RESULTS_FROM_COMPONENT))
                   (qualified-q-2hop
                    (query:Known->Y->X-auto-grow
                     chemical-ids+
                     (set->list
                      (get-non-deprecated/mixin/abstract-ins-and-descendent-predicates*-in-db
                       '("biolink:affects" "biolink:interacts_with")))
                     (set->list
                      (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
                       '("biolink:Gene" "biolink:GeneOrGeneProduct" "biolink:Protein")))
                     '("biolink:affects")
                     gene-category+
                     TOP_BUCKET_NUMBERS
                     MAX_RESULTS_FROM_COMPONENT
                     (lambda (r*) (filter not-semmed-excluded? (mvp2-2hop-filter r* direction))))))
              (list chemical-ids qualified-q-1hop qualified-q-2hop))]
           [(eq? 'mvp2-gene which-mvp)
            ;;
            (define gene-ids
              (hash-ref (hash-ref qg_nodes qg_object-node-id) 'ids))
            (define gene-ids-syns (curies->synonyms-in-db gene-ids))
            (define protein-ids
              (remove-duplicates
               (map car
                    (query:X->Known-scored
                     (set->list
                      (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
                       '("biolink:Protein")))
                     '("biolink:gene_product_of")
                     gene-ids-syns
                     ;; TODO: give names to #f and (list 0) - easy to read
                     (list (list 1112) #f (list 1112))))))
            (define gene-ids+
                (set->list
                 (get-n-descendent-curies*-in-db
                  (append gene-ids-syns (curies->synonyms-in-db protein-ids))
                  MAX_DESCENDENT)))
            (define chemical-catogory+
              (set->list
               (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
                '("biolink:ChemicalEntity"))))
            (define direction
              (let ((qualifer-set
                     (hash-ref (car (hash-ref qg_edge-hash 'qualifier_constraints)) 'qualifier_set)))
                (let loop ((l qualifer-set))
                  (if (equal? (hash-ref (car l) 'qualifier_type_id) "biolink:object_direction_qualifier")
                      (hash-ref (car l) 'qualifier_value)
                      (loop (cdr l))))))
            ;;
            (define 1-hop-proc
              (lambda (score*)
                (filter not-semmed-excluded?
                        (mvp2-1hop-filter
                         (query:X->Known-scored
                          chemical-catogory+
                          '("biolink:affects")
                          gene-ids+
                          score*)
                         direction))))
            ;;
            (let* ((qualified-q-1hop (auto-grow 1-hop-proc TOP_BUCKET_NUMBERS MAX_RESULTS_FROM_COMPONENT))
                   (qualified-q-2hop
                    (query:X->Y->Known-auto-grow
                     chemical-catogory+
                     (set->list
                      (get-non-deprecated/mixin/abstract-ins-and-descendent-predicates*-in-db
                       '("biolink:affects"  "biolink:interacts_with")))
                     (set->list
                      (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
                       '("biolink:Gene" "biolink:GeneOrGeneProduct" "biolink:Protein")))
                     '("biolink:affects")
                     gene-ids+
                     TOP_BUCKET_NUMBERS
                     MAX_RESULTS_FROM_COMPONENT
                     (lambda (r*) (filter not-semmed-excluded? (mvp2-2hop-filter r* direction))))))
              (list gene-ids qualified-q-1hop qualified-q-2hop))])))

      (define q-1hop-unique-results (remove-duplicates q-1hop-results))
      (define q-2hop-unique-results (remove-duplicates q-2hop-results))

      (printf "computed ~s look-up edges and ~s inferred edges for MVP mode creative query\n"
              (length q-1hop-unique-results) (length q-2hop-unique-results))

      (define (score-mvp-edge e)
        (match e
          [`(,curie_x
             ,pred_xy
             ,curie_y
             ,(? string? pred_yz)
             ,(? string? curie_z)
             ,props_xy
             ,props_yz)
           (if (and (edge-has-source? props_xy)
                    (edge-has-source? props_yz))
               (* (num-pubs props_xy) (num-pubs props_yz))
               #f)]
          [`(,curie_x
             ,pred_xy
             ,curie_y
             .
             ,props_xy)
           (if (edge-has-source? props_xy)
               (let ((n (num-pubs props_xy)))
                 (* n n))
               #f)]))

      (define scored/q-1hop-unsorted-long
        (filter (lambda (scored/r) (car scored/r))
                (map
                 (lambda (e) (cons (score-mvp-edge e) e))
                 q-1hop-unique-results)))

      (define scored/q-2hop-unsorted-long
        (filter (lambda (scored/r) (car scored/r))
                (map
                 (lambda (e) (cons (score-mvp-edge e) e))
                 q-2hop-unique-results)))

      (define by-score
        (lambda (score1/e1 score2/e2)
          (let ((score1 (car score1/e1))
                (score2 (car score2/e2)))
            (> score1 score2))))

      (define scored/q-1hop-sorted-long
        (sort scored/q-1hop-unsorted-long by-score))

      (define scored/q-1hop-sorted-short (take-at-most scored/q-1hop-sorted-long (exact-round (/ MAX_RESULTS_FROM_COMPONENT 2.0))))

      (printf "computed ~s valid 1 hop edges for MVP mode creative query\n" (length scored/q-1hop-sorted-short))

      (define scored/q-unsorted-long (append scored/q-1hop-sorted-short scored/q-2hop-unsorted-long))

      (printf "computed total ~s valid edges for MVP mode creative query\n"
              (length scored/q-unsorted-long))

      (define scored/q-sorted-long
        (sort scored/q-unsorted-long by-score))

      (define old-scored/q-sorted-short (take-at-most scored/q-sorted-long MAX_RESULTS_FROM_COMPONENT))

      (define subjs-from-results (remove-duplicates (map cadr old-scored/q-sorted-short)))
      (define objs-from-results (remove-duplicates (map (lambda (e) (get-object e)) old-scored/q-sorted-short)))
      
      (when (eq? which-mvp 'mvp1)
        (let* ((chemicals (remove-duplicates (curies->synonyms-in-db subjs-from-results)))
               (disease-id+ (remove-duplicates (curies->synonyms-in-db objs-from-results)))
               (chem-worsen-disease (remove-duplicates
                                     (curies->synonyms-in-db
                                      (map car 
                                           (time (query:Known->Known
                                            chemicals
                                            '("biolink:causes"
                                              "biolink:exacerbates"
                                              "biolink:has_adverse_event"
                                              "biolink:contributes_to")
                                            disease-id+))))))
               (not-cause-old-scored/q-sorted-short
                (filter (lambda (e) (not (member (cadr e) chem-worsen-disease))) old-scored/q-sorted-short)))
          (set! old-scored/q-sorted-short not-cause-old-scored/q-sorted-short)))

      (printf "Toke the best ~s edges for MVP mode creative query\n"
              (length old-scored/q-sorted-short))

      (define curie-representative-table (add-curies-representative-to-hash
                                          (build-curies-representative-hash subjs-from-results)
                                          objs-from-results))

      (define representative-canonical-table
        (let ((t (make-hash)))
          (define helper
            (lambda (c*)
                (cond
                  ((null? c*) t)
                  ((hash-has-key? t (hash-ref curie-representative-table (car c*)))
                   (helper (cdr c*)))
                  (else
                    (hash-set! t (hash-ref curie-representative-table (car c*)) (car c*))
                    (helper (cdr c*))))))
           (helper (append subjs-from-results objs-from-results))))

      (define curie->canonical
        (lambda (c)
          (hash-ref representative-canonical-table
                    (hash-ref curie-representative-table c))))

      (define scored/q-sorted-short
        (map
         (lambda (e)
                 (match e
                   [`(,score
                      ,curie_x
                      ,pred_xy
                      ,curie_y
                      ,(? string? pred_yz)
                      ,(? string? curie_z)
                      ,props_xy
                      ,props_yz)
                    (list score
                          (curie->canonical curie_x)
                          pred_xy
                          curie_y
                          pred_yz
                          (curie->canonical curie_z)
                          props_xy
                          props_yz)]
                   [`(,score
                      ,curie_x
                      ,pred_xy
                      ,curie_y
                      .
                      ,props_xy)
                    (list* (sqrt score)
                           (curie->canonical curie_x)
                           pred_xy
                           (curie->canonical curie_y)
                           props_xy)]
                   [else (error "invalid form of returned edge" e)]))
         old-scored/q-sorted-short))

      (define representative-score-table
        (cond
          [(or (eq? which-mvp 'mvp1) (eq? which-mvp 'mvp2-gene))
            (let ((subject-score-table (make-hash)))
              (for-each
               (lambda (e)
                 (hash-update! subject-score-table
                               (hash-ref curie-representative-table (cadr e))
                               (lambda (old-socre)
                                 (+ (car e) old-socre))
                               0))
               scored/q-sorted-short)
              subject-score-table)]
          [(eq? which-mvp 'mvp2-chem)
           (let ((object-score-table (make-hash)))
             (for-each
              (lambda (e)
                (hash-update! object-score-table
                              (hash-ref curie-representative-table (get-object e))
                              (lambda (old-socre)
                                (+ (car e) old-socre))
                              0))
              scored/q-sorted-short)
             object-score-table)]
          [else (error "unknown MVP" which-mvp)]))

      (printf "There are ~a unique results/entities for the MVP query.\n" (hash-count representative-score-table))

      (define nodes (make-hash))

      (define edges (make-hash))

      (define auxiliary-graph (make-hash))

      (define unmerged-results (make-hash))

      (define (add-node! curie)
        (let ((props (curie->properties curie)))
          (let* ((categories (list-assoc "category" props))
                 (categories (filter
                              (lambda (c)
                                (not (or
                                      (class-mixin? c)
                                      (class-abstract? c))))
                              categories))
                 (name (get-assoc "name" props)))
            (hash-set! nodes (string->symbol curie)
                       (hash 'categories categories
                             'name name
                             'attributes (list)
                             )))))

      (define (add-edge! subj obj props n)
        (let* ((id
                (or
                 #;(get-assoc "id" props) ; rtx-kg2
                 #;(get-assoc "assertion_id" props) ;text-mining
                 (number->string n)))
               (id (string-append "medik:edge#" id))
               (id-sym (string->symbol id))
               (object obj)
               (subject subj)
               (predicate (get-assoc "predicate" props))
               (aspect-qualifier (get-assoc "object_aspect_qualifier" props))
               (direction-qualifier (get-assoc "object_direction_qualifier" props))
               (qualifed-predicate (get-assoc "qualified_predicate" props))
               (has-pub? (> (num-pubs props) 0)))
          (add-node! object)
          (add-node! subject)
          (unless (hash-has-key? edges id-sym)
            (if (and
                 #;(or (eq? which-mvp 'mvp2-chem) (eq? which-mvp 'mvp2-gene))
                 aspect-qualifier direction-qualifier qualifed-predicate)
                (hash-set! edges id-sym
                           (hash 'attributes
                                 (or
                                  (and (get-assoc "json_attributes" props)
                                       (string->jsexpr (get-assoc "json_attributes" props)))
                                  (data-attributes props has-pub?))
                                 'object object
                                 'predicate predicate
                                 'subject subject
                                 'sources (list (get-source props) UNSECRET-SOURCE)
                                 'qualifiers (list
                                              (hash 'qualifier_type_id "biolink:object_aspect_qualifier"
                                                    'qualifier_value aspect-qualifier)
                                              (hash 'qualifier_type_id "biolink:object_direction_qualifier"
                                                    'qualifier_value direction-qualifier)
                                              (hash 'qualifier_type_id "biolink:qualified_predicate"
                                                    'qualifier_value qualifed-predicate))))
                (hash-set! edges id-sym
                           (hash 'attributes
                                 (or
                                  (and (get-assoc "json_attributes" props)
                                       (string->jsexpr (get-assoc "json_attributes" props)))
                                  (data-attributes props has-pub?))
                                 'object object
                                 'predicate predicate
                                 'subject subject
                                 'sources (list (get-source props) UNSECRET-SOURCE)))))
          id))

      (define (add-creative-edge! sub obj pred n aux-id)
        (let ((id (string-append "medik:creative_edge#" (number->string n))))
          (if (or (eq? which-mvp 'mvp2-gene) (eq? which-mvp 'mvp2-chem))
              (hash-set! edges (string->symbol id)
                         (hash 'attributes
                               (list
                                (auxiliary-graph-attribute aux-id)
                                agent-type-attribute
                                knowledge-level-attribute
                                )
                               'object obj
                               'predicate pred
                               'subject sub
                               'qualifiers
                               (let ((exist-qualifiers (hash-ref (car (hash-ref qg_edge-hash 'qualifier_constraints)) 'qualifier_set))
                                     (qualified_pred (hash 'qualifier_type_id "biolink:qualified_predicate"
                                                           'qualifier_value "biolink:causes")))
                                 (if (member qualified_pred exist-qualifiers)
                                     exist-qualifiers
                                     (cons qualified_pred exist-qualifiers)))
                               'sources (list
                                         (hash
                                          'resource_id "infores:unsecret-agent"
                                          'resource_role "primary_knowledge_source"))))
              (hash-set! edges (string->symbol id)
                         (hash 'attributes
                               (list
                                (auxiliary-graph-attribute aux-id))
                               'object obj
                               'predicate pred
                               'subject sub
                               'sources (list
                                         (hash
                                          'resource_id "infores:unsecret-agent"
                                          'resource_role "primary_knowledge_source")))))
          id))

      (define (add-auxiliary! edge* n)
        (let ((id (string-append "medik:auxiliary_graph#" (number->string n))))
          (hash-set! auxiliary-graph (string->symbol id)
                     (hash 'edges edge*
                           'attributes (list)))
          id))

      (define (add-unmerged-result! r)
        (hash-update! unmerged-results (hash-ref r 'result_id)
                      (lambda (r-old)
                        (let* ((a*-old (hash-ref r-old 'analyses))
                               (a-old (car a*-old))
                               (edge-old (hash-ref (hash-ref a-old 'edge_bindings) qg_edge-id))
                               (edge-new (hash-ref  (hash-ref r 'edge_bindings) qg_edge-id)))
                          (hash-set r-old 'analyses
                                    (list (hash-set (car (hash-ref r-old 'analyses))
                                                    'edge_bindings
                                                    (hash qg_edge-id (remove-duplicates (append edge-old edge-new))))))))
                      (hash
                       'node_bindings (hash-ref r 'node_bindings)
                       'analyses (list (hash 'edge_bindings (hash-ref r 'edge_bindings)
                                             'resource_id "infores:unsecret-agent"
                                             'score (hash-ref representative-score-table (hash-ref r 'score_id)))))))

      (let loop ((en 0) (an 0) (score*/e* scored/q-sorted-short))
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
                ,(? string? pred_yz)
                ,(? string? curie_z)
                ,props_xy
                ,props_yz)
              (if (and (node-has-name-and-cat? curie_x)
                       (node-has-name-and-cat? curie_y)
                       (node-has-name-and-cat? curie_z))
                  (let* ((edge_xy (add-edge! curie_x curie_y props_xy en))
                         (edge_yz (add-edge! curie_y curie_z props_yz (+ en 1)))
                         (auxiliary_id (add-auxiliary! (list edge_xy edge_yz) an))
                         (edge_creative (add-creative-edge! curie_x
                                                            curie_z
                                                            qg_predicate-str
                                                            an
                                                            auxiliary_id)))
                    (add-unmerged-result!
                     (cond
                       [(or (eq? which-mvp 'mvp1) (eq? which-mvp 'mvp2-gene))
                        (hash 'node_bindings
                              (if (equal? curie_z (car input-id*))
                                  (hash
                                   qg_subject-node-id (list (hash 'id curie_x
                                                                  'attributes (list)))
                                   qg_object-node-id (list (hash 'id curie_z
                                                                 'attributes (list))))
                                  (hash
                                   qg_subject-node-id (list (hash 'id curie_x
                                                                  'attributes (list)))
                                   qg_object-node-id (list (hash 'id curie_z
                                                                 'query_id (car input-id*)
                                                                 'attributes (list)))))
                              'result_id (string-append curie_x curie_z)
                              'score_id (hash-ref curie-representative-table curie_x)
                              'edge_bindings (hash qg_edge-id (list (hash 'id edge_creative
                                                                          'attributes (list))))
                              )]
                       [(eq? which-mvp 'mvp2-chem)
                        (hash 'node_bindings
                              (if (equal? curie_x (car input-id*))
                                  (hash
                                   qg_subject-node-id (list (hash 'id curie_x
                                                                  'attributes (list)))
                                   qg_object-node-id (list (hash 'id curie_z
                                                                 'attributes (list))))
                                  (hash
                                   qg_subject-node-id (list (hash 'id curie_x
                                                                  'query_id (car input-id*)
                                                                  'attributes (list)))
                                   qg_object-node-id (list (hash 'id curie_z
                                                                 'attributes (list)))))
                              'result_id (string-append curie_x curie_z)
                              'score_id (hash-ref curie-representative-table curie_z)
                              'edge_bindings (hash qg_edge-id (list (hash 'id edge_creative
                                                                          'attributes (list))))
                              )]
                       [else (error "unknown MVP" which-mvp)]))
                    (loop (+ en 2) (+ an 1) (cdr score*/e*)))
                  (loop en an (cdr score*/e*)))]
             [`(,curie_x
                ,pred_xy
                ,curie_y
                .
                ,props_xy)
              (if (and (node-has-name-and-cat? curie_x)
                       (node-has-name-and-cat? curie_y))
                  (let ((edge_xy (add-edge! curie_x curie_y props_xy en)))
                    (add-unmerged-result!
                     (cond
                       [(and (eq? which-mvp 'mvp1) (not (equal? pred_xy qg_predicate-str)))
                        (let* ((auxiliary_id (add-auxiliary! (list edge_xy) an))
                               (edge_creative (add-creative-edge! curie_x
                                                                  curie_y
                                                                  qg_predicate-str
                                                                  an
                                                                  auxiliary_id)))
                          (hash 'node_bindings
                                (if (equal? curie_y (car input-id*))
                                    (hash
                                     qg_subject-node-id (list (hash 'id curie_x
                                                                    'attributes (list)))
                                     qg_object-node-id (list (hash 'id curie_y
                                                                   'attributes (list))))
                                    (hash
                                     qg_subject-node-id (list (hash 'id curie_x
                                                                    'attributes (list)))
                                     qg_object-node-id (list (hash 'id curie_y
                                                                   'query_id (car input-id*)
                                                                   'attributes (list)))))
                                'result_id (string-append curie_x curie_y)
                                'score_id (hash-ref curie-representative-table curie_x)
                                'edge_bindings (hash qg_edge-id (list (hash 'id edge_creative
                                                                            'attributes (list))))
                                ))]
                       [(or (and (eq? which-mvp 'mvp1) (equal? pred_xy qg_predicate-str))
                            (eq? which-mvp 'mvp2-gene))
                        (hash 'node_bindings
                              (if (equal? curie_y (car input-id*))
                                  (hash
                                   qg_subject-node-id (list (hash 'id curie_x
                                                                  'attributes (list)))
                                   qg_object-node-id (list (hash 'id curie_y
                                                                 'attributes (list))))
                                  (hash
                                   qg_subject-node-id (list (hash 'id curie_x
                                                                  'attributes (list)))
                                   qg_object-node-id (list (hash 'id curie_y
                                                                 'query_id (car input-id*)
                                                                 'attributes (list)))))
                              'result_id (string-append curie_x curie_y)
                              'score_id (hash-ref curie-representative-table curie_x)
                              'edge_bindings (hash qg_edge-id (list (hash 'id edge_xy
                                                                          'attributes (list))))
                              )]
                       [(eq? which-mvp 'mvp2-chem)
                        (hash 'node_bindings
                              (if (equal? curie_x (car input-id*))
                                  (hash
                                   qg_subject-node-id (list (hash 'id curie_x
                                                                  'attributes (list)))
                                   qg_object-node-id (list (hash 'id curie_y
                                                                 'attributes (list))))
                                  (hash
                                   qg_subject-node-id (list (hash 'id curie_x
                                                                  'query_id (car input-id*)
                                                                  'attributes (list)))
                                   qg_object-node-id (list (hash 'id curie_y
                                                                 'attributes (list)))))
                              'result_id (string-append curie_x curie_y)
                              'score_id (hash-ref curie-representative-table curie_y)
                              'edge_bindings (hash qg_edge-id (list (hash 'id edge_xy
                                                                          'attributes (list))))
                              )]))
                    (if (equal? pred_xy qg_predicate-str)
                        (loop (+ en 1) an (cdr score*/e*))
                        (loop (+ en 1) (+ an 1) (cdr score*/e*))))
                  (loop en an (cdr score*/e*)))]
             ))))

      (define merged-results
        (let loop ((id* (hash-keys unmerged-results))
                   (r '()))
          (cond
            [(null? id*) r]
            [else (loop (cdr id*)
                        (cons (hash-ref unmerged-results (car id*)) r))])))

      (define results (sort merged-results (lambda (a b) (> (get-score-from-result a) (get-score-from-result b)))))

      ;; add the input curie/id from query graph to nodes if mediKanren returns answer
      (unless (null? results)
        (add-node! (car input-id*)))

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
               'logs (list)
               'schema_version "1.5.0"
               'biolink_version (get-biolink-version)))

  (list
   'json
   200_OK_STRING
   versioned-trapi-response)
  )


(define (handle-trapi-query body-json request-fk)

  (define (empty-reply)
    (let ()

      (printf "-- handling non-MVP mode query\n")

      (define trapi-response
        (make-empty-trapi-response))

      (list
       'json
       200_OK_STRING
       trapi-response)
      ))

  (define message (hash-ref body-json 'message #f))
  (if message
      (let()
        (define query_graph (hash-ref message 'query_graph #f))
        (if query_graph
            (let ()
              (define edges (hash-ref query_graph 'edges #f))
              (define nodes (hash-ref query_graph 'nodes #f))
              (if (and edges nodes)
                  (let ()
                    (define creative-mvp? (mvp-creative-query? edges nodes))
                    (printf "creative-mvp?: ~s\n" creative-mvp?)
                    (if creative-mvp?
                        (handle-mvp-creative-query body-json message query_graph edges nodes creative-mvp?)
                        (empty-reply)))
                  (let ()
                    (printf "** missing `nodes` or `edges` in `query_graph`: ~s\n" query_graph)
                    (empty-reply))))
            (let ()
              (printf "** missing `query_graph` in `message`: ~s\n" message)
              (empty-reply))))
      (let ()
        (printf "** missing `message` in `body-json`: ~s\n" body-json)
        (empty-reply)))
  )

(define (handle-trapi-asyncquery body-json request-fk)

  (define (empty-reply)
    (let ()

      (printf "-- handling non-MVP mode asyncquery\n")

      (define trapi-response
        (make-empty-trapi-response))

      (list
       'json
       200_OK_STRING
       trapi-response)
      ))

  (define message (hash-ref body-json 'message #f))
  (if message
      (let()
        (define query_graph (hash-ref message 'query_graph #f))
        (if query_graph
            (let ()
              (define edges (hash-ref query_graph 'edges #f))
              (define nodes (hash-ref query_graph 'nodes #f))
              (if (and edges nodes)
                  (let ()
                    (define creative-mvp? (mvp-creative-query? edges nodes))
                    (printf "creative-mvp?: ~s\n" creative-mvp?)
                    (if creative-mvp?
                        (handle-mvp-creative-query body-json message query_graph edges nodes creative-mvp?)
                        (empty-reply)))
                  (let ()
                    (printf "** missing `nodes` or `edges` in `query_graph`: ~s\n" query_graph)
                    (empty-reply))))
            (let ()
              (printf "** missing `query_graph` in `message`: ~s\n" message)
              (empty-reply))))
      (let ()
        (printf "** missing `message` in `body-json`: ~s\n" body-json)
        (empty-reply)))
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
       `(html (body ,(format "Hello, World! from Neo Server ~a. Current amount of jobs waiting: ~a" NEO_SERVER_VERSION *jobs-waiting*)))))
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
               (get-non-deprecated/mixin/abstract-ins-and-descendent-predicates*-in-db
                '("biolink:treats")))
              (set->list
               (get-n-descendent-curies*-in-db
                (curie->synonyms-in-db "DOID:9351")
                MAX_DESCENDENT)))))))))
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
(define q3 (query:X->Known-scored
            (set->list
             (get-non-deprecated/mixin/abstract-ins-and-descendent-classes*-in-db
              '("biolink:ChemicalEntity")))
            (set->list
             (get-non-deprecated/mixin/abstract-ins-and-descendent-predicates*-in-db
              '("biolink:treats")))
            (set->list
             (get-n-descendent-curies*-in-db
              (curie->synonyms-in-db "MONDO:0007827")
              MAX_DESCENDENT))
            TOP_BUCKET_NUMBERS))

(length q3)
(length UNWELCOME-TREATMENT)
