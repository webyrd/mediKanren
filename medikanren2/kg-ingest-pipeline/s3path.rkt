#lang racket
(provide
    (all-defined-out))
(require chk)

(define (s3split-impl path)
    (string-split path "/" #:trim? #f))

(define (s3split-from-uri path-base path)
    (list-tail (s3split-impl path) (length (s3split-impl path-base))))

(define (bucket-from-s3path path)
    (car (s3split-impl path)))

(module+ test
    (chk
        (#:=
            (s3split-from-uri "bucket/ingesttest/tasks"
                "bucket/ingesttest/tasks/kgid/yeast-sri-reference-kg-tsv/v/1.2")
            (list "kgid" "yeast-sri-reference-kg-tsv" "v" "1.2"))))