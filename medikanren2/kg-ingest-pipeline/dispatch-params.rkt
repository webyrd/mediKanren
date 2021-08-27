#lang racket
(provide
    adir-repo-ingest)
;; The directory where ingest logic (e.g. dispatch-build-kg-indexes.rkt) should find 
;; its scripts and other resources.
(define adir-repo-ingest (make-parameter 'adir-repo-ingest-placeholder))

