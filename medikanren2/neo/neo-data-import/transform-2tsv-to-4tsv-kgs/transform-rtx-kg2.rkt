#lang racket

(require "transform-generic.rkt"
         "transform-edge-tsv.rkt"
         "transform-node-tsv.rkt")

(define BASE "rtx-kg2-v2.10.0/")

(transform-generic (string-append "../../neo-data/raw_downloads_from_kge_archive/" BASE)
                   (string-append "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/" BASE)
                   "data_01_RAW_KGs_rtx_kg2_v2.10.0_validated_rtx-kg2_2.10.0_nodes.tsv"
                   "data_01_RAW_KGs_rtx_kg2_v2.10.0_validated_rtx-kg2_2.10.0_edges.tsv"
                   "rtx_kg2"
                   'rtx-kg2
                   (cons transform-node-tsv transform-edge-tsv))
