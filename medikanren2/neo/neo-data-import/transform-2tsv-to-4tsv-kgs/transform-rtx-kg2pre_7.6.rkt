#lang racket
(require "transform.rkt")

(transform "../../neo-data/raw_downloads_from_kge_archive/rtx-kg2pre_7.6/"
           "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/rtx-kg2pre_7.6/"
           "nodes_dos_free.tsv"
           "edges.tsv"
           "rtx-kg2pre_7.6")
