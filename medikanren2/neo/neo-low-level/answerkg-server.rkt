#lang racket

;; the skeleton for this server is taken from
;; https://github.com/CHARM-BDF/mediKanren-MCP

(define port
  (let ((port (getenv "ANSWERKG_MEDIKANREN_PORT")))
    (if port
        (string->number port)
        8181)))

(define max-worker-count
  (let ((count (getenv "MAX_WORKER_COUNT")))
    (if count
        (string->number count)
        5)))

(define worker-semaphore (make-semaphore max-worker-count))

(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/http/bindings)
(require web-server/http/json)
(require json)
(require "answerkg.rkt")

(require racket/engine)
(define (job-failure x)
  (response/xexpr x))

(define timeout (* 10 1000)) ;; in miliseconds
(define max-waiting (+ timeout 500))

(define (thunk-with-timeout thunk)
  (lambda ()
    (define e (engine (lambda (x) (thunk))))
    (if (engine-run timeout e)
        (engine-result e)
        (begin
          (displayln "timeout")
          (job-failure "timeout")))))

(define (work-safely work)
  (define custodian.work (make-custodian))
  (define result
    ;; current-custodian will collect all file handles opened during work
    (parameterize ((current-custodian custodian.work))
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
                     (call-in-nested-thread work custodian.work))))
  (custodian-shutdown-all custodian.work) ; close all file handles opened during work
  result)
(define (handle-work-safely handle-request)
  (lambda (request)
    (call-with-semaphore
     worker-semaphore
     (thunk-with-timeout
      (lambda ()
        (work-safely (lambda () (handle-request request))))))))
(define w handle-work-safely)

(define (start request)
  (server-dispatch request))

(define-values (server-dispatch server-url)
  (dispatch-rules
   (("") (w handle-index-request))
   (("query") (w handle-query-request))
   (else (w handle-index-request))))

(define (handle-index-request request)
  (response/xexpr
   `(html (body
           (div
            (h1 "Welcome to the answerkg FLT1 Racket server!")
            (p " The endpoint is:"
               (ul
                (li "query?subject=...&predicate=...&object=... " (a ([href "/query?subject=&predicate=&object=NCBIGene:4627"]) "(example)")))))
))))

(define (handle-query-request request)
  (define params (request-bindings request))
  (response/jsexpr
   (query
    (maybe-cdr (assoc 'subject params)) (maybe-cdr (assoc 'predicate params)) (maybe-cdr (assoc 'object params)))))

(define (maybe-cdr x)
  (if x (cdr x) ""))

(define (send-ready-signal)
  ;; Sends signal to `pm2` when the server is ready
  (system "kill -s SIGUSR2 $PM2_PID"))

(define (serve)
  (serve/servlet start
                 #:servlet-path ""
                 #:port port
                 #:servlet-regexp #rx""
                 #:max-waiting max-waiting
                 #:launch-browser? #false)
  (send-ready-signal))
(serve)
