#lang racket

(require "transform-generic.rkt"
         "transform-edge-jsonl.rkt"
         "transform-node-jsonl.rkt")

(define BASE "rtx-kg2-2.10.0pre/")

(transform-generic (string-append "../../neo-data/raw_downloads_from_kge_archive/" BASE)
                   (string-append "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/" BASE)
                   "kg2.10.0pre-nodes.jsonl"
                   "kg2.10.0pre-edges.jsonl"
                   "rtx_kg2"
                   'rtx-kg2
                   (cons transform-node-jsonl transform-edge-jsonl))