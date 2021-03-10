#lang racket

(require
  "create-hashtable-common.rkt")

(provide
  (all-defined-out))


(define hgnc-ht (make-hash))
(define hgnc-ht-file-path (build-path HASHTABLE_SAVE_DIRECTORY "hgnc-hash.rkt"))

(define (fill-hgnc-ht!)

  (set! hgnc-ht (make-hash))

  (define add-to-ht! (add-concept-key/cid-associations-to-hashtable! hgnc-ht))

  (add-to-ht! SEMMED #rx"HGNC:([0-9]+)")
  ;; apparently rtx doesn't have HGNC synonyms 
  (add-to-ht! ROBOKOP #rx"HGNC:([0-9]+)")
  (add-to-ht! ORANGE #rx"HGNC:([0-9]+)")

  )

(define (save-hgnc-ht!)
  (save-hashtable! hgnc-ht hgnc-ht-file-path))

(define (load-hgnc-ht)
  (load-hashtable hgnc-ht-file-path))

(define (load-or-create/save-hgnc-ht!)
  (load-or-create/save-hashtable!
    'hgnc
    load-hgnc-ht
    fill-hgnc-ht!
    save-hgnc-ht!))
