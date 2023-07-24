#lang racket
(require "transform-generic.rkt"
         "transform-edge-tsv.rkt"
         "transform-node-tsv.rkt")

(transform-generic "../../neo-data/raw_downloads_from_kge_archive/kg2-may-2023/"
                   "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/kg2-may-2023/"
                   "nodes_with_header.tsv"
                   "edges_with_header.tsv"
                   "rtx_kg2"
                   'rtx-kg2
                   (cons transform-node-tsv transform-edge-tsv))
