#lang racket

(require "transform-generic.rkt"
         "transform-edge-tsv.rkt"
         "transform-node-tsv.rkt"
         "transform-edge-jsonl.rkt"
         "transform-node-jsonl.rkt"
         racket/runtime-path)

(provide transform-temporary-kg)

(define-runtime-path path.here ".")
(define (transform-temporary-kg raw-data-folder-name nodes-file-name edges-file-name)


  (define raw-data-path (path->string (build-path path.here (string-append "../../neo-data/raw_downloads_from_kge_archive/" raw-data-folder-name))))
  (define transformed-data-path (path->string (build-path path.here (string-append "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/" raw-data-folder-name))))
  (define intermediate-name-base "temporary_kg")

  (when (not (directory-exists? transformed-data-path))
    (make-directory transformed-data-path))

  (cond
    [(and (string-suffix? edges-file-name ".tsv")
          (string-suffix? nodes-file-name ".tsv"))
     (transform-no-bucket raw-data-path
                          transformed-data-path
                          nodes-file-name
                          edges-file-name
                          intermediate-name-base
                          (cons transform-node-tsv transform-edge-tsv-no-bucket))]
    [(and (string-suffix? edges-file-name ".jsonl")
          (string-suffix? nodes-file-name ".jsonl"))
     (transform-no-bucket raw-data-path
                          transformed-data-path
                          nodes-file-name
                          edges-file-name
                          intermediate-name-base
                          (cons transform-node-jsonl transform-edge-jsonl-no-bucket))]
    [else (error "Please check if your edge and node files are in the TSV or JSONL formats")]))
