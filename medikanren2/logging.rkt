#lang racket/base
(provide log-time log-info log-error log-once log-length)
(require
  racket/file racket/function racket/list racket/hash
  (except-in racket/match ==)
  racket/port
  racket/pretty
  racket/runtime-path
  racket/string
  json
  memoize
  racket/format
  racket/date
  )

(date-display-format 'iso-8601)

(define-syntax log-time
  (syntax-rules ()
    ((_ log-proc log-key label body)
     (let-values (((result cpu real gc) (time-apply (lambda () body) '())))
       (log-proc log-key label cpu real (car result))
       (car result)))))

(define (log-info key message)
  (printf "~a    ~s    ~a       ~a\n"
          (date->string (seconds->date (current-seconds)) #t)
          key
          "INFO"
          message))

(define (log-error key message)
  (printf "~a    ~s    ~a       ~a\n"
          (date->string (seconds->date (current-seconds)) #t)
          key
          "ERROR"
          message))

(define/memo* (log-once key label cpu real result)
  (log-info key (format "~a [cpu time: ~s real time: ~s]: ~s" label cpu real result)))

(define (log-length key label cpu real results)
  (log-info key (format "~a [cpu time: ~s real time: ~s]: ~s" 
                        label cpu real (length (hash-ref  results 'results '())))))
