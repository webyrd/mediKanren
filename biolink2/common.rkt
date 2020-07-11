#lang racket/base
(provide
  (all-from-out "dbk/dbk.rkt")
  path/data path/root path.data path.root
  )
(require "dbk/dbk.rkt" racket/runtime-path)

(define-runtime-path path.root ".")
(define (path/root relative-path)
  (path->string (build-path path.root relative-path)))
(define path.data (path/root "data"))
(define (path/data relative-path)
  (path->string (build-path path.data relative-path)))
