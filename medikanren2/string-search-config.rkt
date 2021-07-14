#lang racket
;; This file is private to string-search.rkt.
;; Applications should include string-search.rkt instead.
(provide
    field-indexed
    schema-pri
    name-ornull-from-pri
    name-from-pri
    uri-from-pri
    fn-concept-name-corpus
    fn-concept-name-index
    fn-cprop-primary
)

;;;; Isolate the hard-coded assumptions of string-search here.

;; For now, we only index the field called "name"
(define field-indexed "name")

(define schema-pri '(string string string))

;;; *** data transformation for ingest ***
(define (name-ornull-from-pri v)
  (let* (
      (fd (list-ref v 1))
      (name (list-ref v 2)))
    (if (equal? fd field-indexed)
      `(,name)
      '())))

;;; *** data transformation for egress ***
(define (name-from-pri pri)
  (list-ref pri 2))

(define (uri-from-pri pri)
  (list-ref pri 0))


;;;; Filenames

(define fn-concept-name-corpus "concept-name-corpus.scm")     ; only for build-name-string-search-v1
(define fn-concept-name-index  "concept-name-index.bytes")
(define fn-cprop-primary "primary.value.table")

