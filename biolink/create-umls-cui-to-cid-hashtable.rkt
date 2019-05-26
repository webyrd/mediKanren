#lang racket

(require
  "create-hashtable-common.rkt")

(provide
  (all-defined-out))


(define umls-ht (make-hash))
(define umls-ht-file-path (build-path HASHTABLE_SAVE_DIRECTORY "umls-hash.rkt"))

(define (fill-umls-ht!)

  (set! umls-ht (make-hash))

  (define add-to-ht! (add-concept-key/cid-associations-to-hashtable! umls-ht))

  (add-to-ht! SEMMED #rx"UMLS:([A-Z0-9]+)")
  ;; Apparently RTX doesn't have UMLS mappings
  (add-to-ht! ROBOKOP #rx"UMLS:([A-Z0-9]+)")
  ;; Apparently Orange doesn't have UMLS mappings

  )

(define (save-umls-ht!)
  (save-hashtable! umls-ht umls-ht-file-path))

(define (load-umls-ht)
  (load-hashtable umls-ht-file-path))

(define (load-or-create/save-umls-ht!)
  (load-or-create/save-hashtable!
    'umls
    load-umls-ht
    fill-umls-ht!
    save-umls-ht!))
