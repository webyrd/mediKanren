#lang racket/base
(provide
  pretty-timestamp
  pretty-log/port
  pretty-logf/port
  pretty-log
  pretty-logf
  time/pretty-log
  current-log-port)
(require racket/pretty)

;; TODO: eventually replace the logging in config.rkt with these definitions
(define (pretty-timestamp)
  (define seconds (current-seconds))
  (define d       (seconds->date seconds #f))
  (list seconds 'UTC
        (date-year d) (date-month  d) (date-day    d)
        (date-hour d) (date-minute d) (date-second d)))

(define (pretty-log/port  out         . args) (pretty-write    (cons (pretty-timestamp) args) out))
(define (pretty-logf/port out message . args) (pretty-log/port out (apply format message args)))

(define current-log-port (make-parameter (current-error-port)))

(define (pretty-log  . args) (apply pretty-log/port  (current-log-port) args))
(define (pretty-logf . args) (apply pretty-logf/port (current-log-port) args))

(define-syntax-rule (time/pretty-log body ...)
  (let-values (((results time.cpu time.real time.gc) (time-apply (lambda () body ...) '())))
    (pretty-log `(time cpu ,time.cpu real ,time.real gc ,time.gc))
    (apply values results)))
