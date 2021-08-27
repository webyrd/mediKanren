#lang racket
(provide
    with-config
    uri-kge-base
    s3path-base
    uri-webhook-slack)
(require "current-source.rkt")
(require json)

(define uri-kge-base (make-parameter #f))
(define s3path-base (make-parameter #f))
(define uri-webhook-slack (make-parameter #f))

(define (with-config thunk-run)
  (define afile-config 
    (simplify-path (build-path (adir-current-source) "config.json")))
  (define st-token (file->string afile-config))
  (define jsexpr (string->jsexpr st-token))
  (parameterize ((uri-kge-base (dict-ref jsexpr 'uri-kge))
                 (s3path-base (dict-ref jsexpr 's3path-prefix))
                 (uri-webhook-slack (dict-ref jsexpr 'uri-webhook-slack)))
    (thunk-run)))


