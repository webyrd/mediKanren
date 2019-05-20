#lang racket

(require
  "create-hashtable-common.rkt"
  "create-ensembl-cui-to-cid-hashtable.rkt"
  "create-hgnc-cui-to-cid-hashtable.rkt"
  "create-uniprotkb-cui-to-cid-hashtable.rkt")

(provide
  (all-defined-out))

(define my-ensembl-ht (load-or-create/save-ensembl-ht!))
(define my-hgnc-ht (load-or-create/save-hgnc-ht!))
(define my-uniprotkb-ht (load-or-create/save-uniprotkb-ht!))
