#lang racket/base
(require "../dbk/storage.rkt"
         racket/pretty
         racket/runtime-path)

(define-runtime-path path.here ".")

(define stg (storage:filesystem (build-path path.here "example-storage")))
;(define stg2 (storage:filesystem (build-path path.here "example-storage")))

(storage-block-remove-names! stg '(testing 2) '(new 1) '(new 2.1) '(new 2.2))

(storage-checkpoint! stg)

(pretty-write (storage-description-keys stg))
(pretty-write (storage-block-names stg))

;(define (sset! . kvs) (apply storage-description-set! stg kvs))
;(define (sref  . ks)  (map cons ks (map (lambda (k) (storage-description-ref stg k)) ks)))

;(define out.1 (storage-block-new! stg '(testing 1)))
;(pretty-write (equal? out.1 (storage-block-out stg '(testing 1))))

(storage-block-new! stg '(testing 2))

(call-with-output-file
  (storage-block-new! stg '(testing 1))
  (lambda (out) (write-string "this is testing 1" out)))
(call-with-output-file
  (storage-block-path stg '(testing 2))
  (lambda (out) (write-string "this is testing 2" out)))

(pretty-write (storage-block-path stg '(testing 2)))

(storage-block-rename! stg '(testing 1) '(new 1))
(storage-block-add-names! stg '(testing 2) '(new 2.1) '(new 2.2))

(storage-checkpoint! stg)

(pretty-write (storage-description-keys stg))
(pretty-write (storage-block-names stg))
(pretty-write (map (lambda (name) (storage-block-path stg name)) (storage-block-names stg)))

(pretty-write `(in.1:   ,(call-with-input-file (storage-block-path stg '(new 1))     read-line)))
(pretty-write `(in.2:   ,(call-with-input-file (storage-block-path stg '(testing 2)) read-line)))
(pretty-write `(in.2.1: ,(call-with-input-file (storage-block-path stg '(new 2.1))   read-line)))
(pretty-write `(in.2.2: ,(call-with-input-file (storage-block-path stg '(new 2.2))   read-line)))

(storage-block-remove-names! stg '(testing 2) '(new 1) '(new 2.1) '(new 2.2))

(storage-revert!     stg)
(storage-checkpoint! stg)

;(sset! 'hello 1 'world! 2)

;(storage-checkpoint!  stg)
;(storage-revert!      stg)
;(pretty-write (storage-checkpoint-count stg))
;(pretty-write (sref 'hello 'world!))

;(sset! 'hello 1 'world! 2)

;(storage-checkpoint!  stg)
;(storage-revert!      stg)
;(pretty-write (storage-checkpoint-count stg))
;(pretty-write (sref 'hello 'world!))

;(sset! 'hello 5 'world! 2)

;(storage-revert!      stg)
;(storage-checkpoint!  stg)
;(pretty-write (storage-checkpoint-count stg))
;(pretty-write (sref 'hello 'world!))

;(sset! 'hello 5 'world! 2)

;(storage-checkpoint!  stg)
;(storage-revert!      stg)
;(pretty-write (storage-checkpoint-count stg))
;(pretty-write (sref 'hello 'world!))

(storage-trash-empty! stg)
