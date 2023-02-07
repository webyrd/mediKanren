#lang racket

(require "transform-generic.rkt"
         "transform-edge-jsonl.rkt"
         "transform-node-jsonl.rkt")

(transform-generic "../../neo-data/raw_downloads_from_kge_archive/full_RobokopKG_graph/"
           "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/full_Robokop/"
           "nodes.jsonl"
           "edges.jsonl"
           "full_Robokop"
           (cons transform-node-jsonl transform-edge-jsonl))
