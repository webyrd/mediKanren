#lang racket

(require
  "create-hashtable-common.rkt"
  "create-ensembl-cui-to-cid-hashtable.rkt"
  "create-hgnc-cui-to-cid-hashtable.rkt"
  "create-hpo-cui-to-cid-hashtable.rkt"
  "create-mondo-cui-to-cid-hashtable.rkt"
  "create-omim-cui-to-cid-hashtable.rkt"
  "create-uniprotkb-cui-to-cid-hashtable.rkt")

(provide
  (all-defined-out))

(define ensembl-ht #f)
(define hgnc-ht #f)
(define hpo-ht #f)
(define omim-ht #f)
(define mondo-ht #f)
(define uniprotkb-ht #f)

(define (load-or-create/save-all-hashtables!)

  (set! ensembl-ht (load-or-create/save-ensembl-ht!))
  (set! hgnc-ht (load-or-create/save-hgnc-ht!))
  (set! hpo-ht (load-or-create/save-hpo-ht!))
  (set! mondo-ht (load-or-create/save-mondo-ht!))
  (set! omim-ht (load-or-create/save-omim-ht!))
  (set! uniprotkb-ht (load-or-create/save-uniprotkb-ht!))
  
  )
