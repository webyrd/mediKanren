#lang racket

(require
  "create-hashtable-common.rkt")

(provide
  (all-defined-out))

(define mondo-ht (make-hash))
(define mondo-ht-file-path (build-path HASHTABLE_SAVE_DIRECTORY "mondo-hash.rkt"))

(define (fill-mondo-ht!)

  (set! mondo-ht (make-hash))

  (define add-to-ht! (add-concept-key/cid-associations-to-hashtable! mondo-ht))

  ;; Apparently Semmed doesn't have MONDO mappings
  (add-to-ht! RTX #rx"MONDO:([A-Z0-9]+)")
  (add-to-ht! ROBOKOP #rx"MONDO:([A-Z0-9]+)")
  (add-to-ht! ORANGE #rx"MONDO:([A-Z0-9]+)")

  )

(define (save-mondo-ht!)
  (save-hashtable! mondo-ht mondo-ht-file-path))

(define (load-mondo-ht)
  (load-hashtable mondo-ht-file-path))

(define (load-or-create/save-mondo-ht!)
  (load-or-create/save-hashtable!
    'mondo
    load-mondo-ht
    fill-mondo-ht!
    save-mondo-ht!))
