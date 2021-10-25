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
  json
)

(define seconds-per-query (make-parameter 60))

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

(define (call-trapi fn msg)
    (printf "starting fn=~a\n" fn)
    (flush-output (current-output-port))
    (define out (with-timeout (seconds-per-query) (lambda () (trapi-response msg (current-seconds)))))
    (printf "completed fn=~a\n" fn)
    (flush-output (current-output-port))
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
                    (in (file->string fn))
                    (jsexpr (string->jsexpr in))
                    (trapimsg (hash-ref jsexpr 'message))
                    (t0 (current-milliseconds))
                    (out (call-trapi fn trapimsg))
                    (dt (exact->inexact (/ (- (current-milliseconds) t0) 1000)))
                    (inout `((fn . ,fn) (dt . ,dt) (in . ,in) (out . ,out))))
                (iter (cons inout inouts)))))))
    (iter '()))

(define (num-results-from-out a1)
    (if (null? a1)
        -1                               ; -1 indicates timeout
        (length (hash-ref (car a1) 'results))))

(define (run-main)
    (let ((inouts (read-and-run-by-filename (current-input-port))))
        (for ((inout inouts))
            (let* (
                    (n (num-results-from-out (dict-ref inout 'out))))
                (printf "num-answers=~a dt=~a for ~a\n"
                    n
                    (dict-ref inout 'dt)
                    (dict-ref inout 'fn)))))
)

(define (parse-configuration)
  (command-line
   #:program "trapi-cli.rkt"
   #:usage-help "trapi-cli.rkt <options>"
   #:once-each
   [("--seconds-per-query") sec
                    "Number of seconds to allow for each query"
                    (seconds-per-query (string->number (string-trim #:left? #f sec)))]
   #:args ()
   '())
   ;""  ; We don't care about the return value because configuration lives in mutatable racket parameters
  )

(module+ main
  (parse-configuration)
  (run-main)
  )


