#lang racket/base
(require
  "common.rkt"
  racket/pretty
  racket/runtime-path
  json
  )

(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define-runtime-path path:root ".")
(define (path/root relative-path) (build-path path:root relative-path))

(define schema.json
  (call-with-input-file (path/root "open-api/TranslatorReasonersAPI.json")
                        read-json))

(pretty-print schema.json)
(pretty-print (hash-keys schema.json))
