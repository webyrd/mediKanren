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
  json
)

(define (call-trapi fn msg)
    (printf "starting fn=~a\n" fn)
    (flush-output (current-output-port))
    (define out (trapi-response msg (current-seconds)))
    (printf "completed fn=~a\n" fn)
    (flush-output (current-output-port))
    out)

(define (read-and-run-by-filename fd)
    (define (iter inouts)
        (define l (read-line fd 'any))
        (if (eof-object? l)
            inouts
            (let* (
                    (fn (string-trim l))
                    (in (file->string fn))
                    (jsexpr (string->jsexpr in))
                    (trapimsg (hash-ref jsexpr 'message))
                    (t0 (current-milliseconds))
                    (out (call-trapi fn trapimsg))
                    (dt (exact->inexact (/ (- (current-milliseconds) t0) 1000)))
                    (inout `((fn . ,fn) (dt . ,dt) (in . ,in) (out . ,out))))
                (iter (cons inout inouts)))))
    (iter '()))

(define (results-from-out a1)
    (hash-ref a1 'results))

(let ((inouts (read-and-run-by-filename (current-input-port))))
    (for ((inout inouts))
        (let* (
                (results (results-from-out (dict-ref inout 'out)))
                (n (length results)))
            (printf "num-answers=~a dt=~a for ~a\n"
                n
                (dict-ref inout 'dt)
                (dict-ref inout 'fn)))))
            






