#lang racket

(require
  "create-hashtable-common.rkt")

(provide
  (all-defined-out))


(define doid-ht (make-hash))
(define doid-ht-file-path (build-path HASHTABLE_SAVE_DIRECTORY "doid-hash.rkt"))

(define (fill-doid-ht!)

  (set! doid-ht (make-hash))

  (define add-to-ht! (add-concept-key/cid-associations-to-hashtable! doid-ht))

  (add-to-ht! SEMMED #rx"DOID:([0-9]+)")
  (add-to-ht! RTX #rx"DOID:([0-9]+)")
  (add-to-ht! ROBOKOP #rx"DOID:([0-9]+)")
  (add-to-ht! ORANGE #rx"DOID:([0-9]+)")

  )

(define (save-doid-ht!)
  (save-hashtable! doid-ht doid-ht-file-path))

(define (load-doid-ht)
  (load-hashtable doid-ht-file-path))

(define (load-or-create/save-doid-ht!)
  (load-or-create/save-hashtable!
    'doid
    load-doid-ht
    fill-doid-ht!
    save-doid-ht!))
