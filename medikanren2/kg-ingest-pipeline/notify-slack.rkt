#lang racket
(provide
    (all-defined-out))
(require http/request)
(require json)
(require "main-params.rkt")

(define http-version "1.1")

(define (notify-slack msg)
    (define bytes (jsexpr->bytes 
        `#hash(
            (channel . "medikanren-dev-notify-ingest")
            (text . ,msg)
        )))
    (define heads '())
    (call/output-request http-version
                            "POST"
                            (uri-webhook-slack)
                            bytes
                            #f
                            heads
                            read-entity/bytes
                            #:redirects 0))

