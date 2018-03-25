#lang racket/base
(provide

  )

(require
  "mk.rkt"
  "mk-parse.rkt"
  )

;; TODO: Process SQL schema

;; Goals:
;; raw mappings for SQL tables
;; foreign key cross-references
;; mk relations that resolve foreign keys
;; later: incorporate indices
