#lang racket

(require
  "create-hashtable-common.rkt")

(provide
  (all-defined-out))


(define hpo-ht (make-hash))
(define hpo-ht-file-path (build-path HASHTABLE_SAVE_DIRECTORY "hpo-hash.rkt"))

(define (fill-hpo-ht!)

  (set! hpo-ht (make-hash))

  (define add-to-ht! (add-concept-key/cid-associations-to-hashtable! hpo-ht))

  (add-to-ht! SEMMED #rx"HPO:HP:([0-9]+)")
  (add-to-ht! RTX #rx"HP:([0-9]+)")
  (add-to-ht! ROBOKOP #rx"HP:([0-9]+)")
  (add-to-ht! ORANGE #rx"HP:([0-9]+)")

  )

(define (save-hpo-ht!)
  (save-hashtable! hpo-ht hpo-ht-file-path))

(define (load-hpo-ht)
  (load-hashtable hpo-ht-file-path))

(define (load-or-create/save-hpo-ht!)
  (load-or-create/save-hashtable!
    'hpo
    load-hpo-ht
    fill-hpo-ht!
    save-hpo-ht!))
