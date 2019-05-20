#lang racket

(require
  "create-hashtable-common.rkt")

(provide
  (all-defined-out))

(define omim-ht (make-hash))
(define omim-ht-file-path (build-path HASHTABLE_SAVE_DIRECTORY "omim-hash.rkt"))

(define (fill-omim-ht!)

  (set! omim-ht (make-hash))

  (define add-to-ht! (add-concept-key/cid-associations-to-hashtable! omim-ht))

 (add-to-ht! SEMMED #rx"OMIM:([0-9]+)") 
  ;; apparently rtx doesn't have OMIM synonyms
  (add-to-ht! ROBOKOP #rx"OMIM:([0-9]+)")
  (add-to-ht! ORANGE #rx"OMIM:([0-9]+)")

  )

(define (save-omim-ht!)
  (save-hashtable! omim-ht omim-ht-file-path))

(define (load-omim-ht)
  (load-hashtable omim-ht-file-path))

(define (load-or-create/save-omim-ht!)
  (load-or-create/save-hashtable!
    'omim
    load-omim-ht
    fill-omim-ht!
    save-omim-ht!))



#|

|#
