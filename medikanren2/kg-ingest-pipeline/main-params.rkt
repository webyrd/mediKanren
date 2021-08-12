#lang racket
(provide
    config
    with-config
    s3path-base)
(require "current-source.rkt")
(require json)

(define config (make-parameter 'config-placeholder))

(define (with-config thunk-run)
  (define afile-config 
    (simplify-path (build-path (adir-current-source) "config.json")))
  (define st-token (file->string afile-config))
  (define jsexpr (string->jsexpr st-token))
  (printf "loaded config: ~s\n" jsexpr)
  (parameterize ((config jsexpr))
    (thunk-run)))

(define s3path-base (make-parameter 's3path-base-placeholder))

