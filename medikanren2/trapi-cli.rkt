#lang racket

#|
    Reads TRAPI filenames from stdin and runs them.  Example (run all Dec 2021 queries):

        git clone https://github.com/NCATSTranslator/minihackathons
        find minihackathons -maxdepth 3 -type f -path \*2021-12_demo\* -name \*.json \
            | racket -l errortrace -u medikanren2/trapi-cli.rkt

|#

(require
 "common.rkt"   
  "trapi.rkt"
  "logging.rkt"
  racket/file racket/function racket/list racket/hash
  (except-in racket/match ==)
  racket/pretty
  racket/runtime-path
  racket/dict
  racket/async-channel
  http/request
  json
)

(define seconds-per-query (make-parameter 60))
(define seconds-idle-between-queries (make-parameter 15))
(define uri-trapi (make-parameter #f))

(define (with-timeout seconds thunk)
  (define ach (make-async-channel))
  (define t (thread
              (lambda ()
                (async-channel-put
                    ach
                    (list (thunk))))))    ; nonempty list indicates success
  ; Watcher thread:
  (thread (lambda ()
            (sleep seconds)
            (kill-thread t)
            (async-channel-put ach '()))) ; empty list indicates timeout
  (sync ach))

(define (run-query-without-network-impl fn msg)
    (flush-output (current-output-port))
    (define maybe-out (with-timeout (seconds-per-query) (lambda () (trapi-response msg))))
    (flush-output (current-output-port))
    (if (null? maybe-out)
        (hasheq 'results '())
        (let* ((out (car maybe-out))
                (r (dict-ref out 'results)))
            (dict-set out 'results r))))


(define (run-query-without-network fn json1)
    (define jsexpr (string->jsexpr json1))
    (define trapimsg (hash-ref jsexpr 'message))
    (define out (run-query-without-network-impl fn trapimsg))
    (hasheq 'message out))

(define http-version "1.1")
(define (run-query-with-network fn json1)
    (define uri (format "~a~a" (uri-trapi) "/query"))
    (define headers '((#"Content-Type" . "application/json")))
    (define bytes-out (call/output-request http-version
                            "POST"
                            uri
                            (string->bytes/utf-8 json1)
                            #f
                            headers
                            read-entity/bytes
                            #:redirects 0))
    (define tmp (bytes->jsexpr bytes-out))
    (sleep (seconds-idle-between-queries))
    tmp)

(define (run-query fn json1)
    (displayln (jsexpr->string (hasheq
        'event "request"
        'fn fn)))
    (define out
        (if (uri-trapi)
            (run-query-with-network fn json1)
            (run-query-without-network fn json1)))
    (displayln (jsexpr->string (hasheq
        'event "response"
        'fn fn
        'out out
    )))
    out)

(define (read-and-run-by-filename fd)
    (define (iter inouts)
        (define l (read-line fd 'any))
        (if (eof-object? l)
            inouts
            (let ((fn (string-trim l)))
            (if (or (<= (string-length fn) 1) (not (file-exists? fn)))
                '()
                (let* (
                    (t0 (current-milliseconds))
                    (json1 (file->string fn))
                    (out (run-query fn json1))
                    (dt (exact->inexact (/ (- (current-milliseconds) t0) 1000)))
                    (inout `((fn . ,fn) (dt . ,dt) (json . ,json1) (out . ,out))))
                (iter (cons inout inouts)))))))
    (iter '()))

(define (num-results-from-out a1)
;    (printf "a1=\n")
;    (pretty-write a1)
    (if (null? a1)
        -1                               ; -1 indicates timeout
        (length (dict-ref (dict-ref a1 'message) 'results))))

(define (run-main)
    (let ((inouts (read-and-run-by-filename (current-input-port))))
        (for ((inout inouts))
            (let* (
                    (n (num-results-from-out (dict-ref inout 'out))))
                (displayln
                    (jsexpr->string
                        (hasheq
                            'event "summary"
                            'num-answers n
                            'dt (dict-ref inout 'dt)
                            'fn (dict-ref inout 'fn)))))))
)

(define (parse-configuration)
  (command-line
   #:program "trapi-cli.rkt"
   #:usage-help "trapi-cli.rkt <options>"
   #:once-each
   [("--seconds-per-query") sec
                    "Number of seconds to allow for each query"
                    (seconds-per-query (string->number (string-trim #:left? #f sec)))]
   [("--uri-trapi") adir
                    "A URI to a TRAPI service"
                    (uri-trapi (string-trim #:left? #f adir))]
   #:args ()
   '())
   ;""  ; We don't care about the return value because configuration lives in mutatable racket parameters
  )

(module+ main
  (parse-configuration)
  (run-main)
  )


