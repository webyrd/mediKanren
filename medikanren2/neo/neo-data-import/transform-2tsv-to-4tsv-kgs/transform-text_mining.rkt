#lang racket

(require "transform-generic.rkt"
         "transform-edge-tsv.rkt"
         "transform-node-tsv.rkt")

(define BASE "text-mining-aug-5-2024/")

(transform-generic (string-append "../../neo-data/raw_downloads_from_kge_archive/" BASE)
                   (string-append "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/" BASE)
                   "nodes_with_header.tsv"
                   "edges_with_header.tsv"
                   "text_mining"
                   'text-mining
                   (cons transform-node-tsv transform-edge-tsv))