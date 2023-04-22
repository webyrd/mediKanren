#lang racket

(require "transform-generic.rkt"
         "transform-edge-tsv.rkt"
         "transform-node-tsv.rkt")

(transform-generic "../../neo-data/raw_downloads_from_kge_archive/text-mining-march-20/"
                   "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/text-mining-march-20/"
                   "nodes_with_header.tsv"
                   "edges_with_header.tsv"
                   "text_mining"
                   (cons transform-node-tsv transform-edge-tsv))
