#lang racket

(require
  "create-hashtable-common.rkt"
  "create-ensembl-cui-to-cid-hashtable.rkt"
  "create-hgnc-cui-to-cid-hashtable.rkt"
  "create-omim-cui-to-cid-hashtable.rkt"
  "create-uniprotkb-cui-to-cid-hashtable.rkt")

(provide
  (all-defined-out))

(define ensembl-ht #f)
(define hgnc-ht #f)
(define omim-ht #f)
(define uniprotkb-ht #f)

(define (load-or-create/save-all-hashtables!)

  (set! ensembl-ht (load-or-create/save-ensembl-ht!))
  (set! hgnc-ht (load-or-create/save-hgnc-ht!))
  (set! omim-ht (load-or-create/save-omim-ht!))
  (set! uniprotkb-ht (load-or-create/save-uniprotkb-ht!))
  
  )
