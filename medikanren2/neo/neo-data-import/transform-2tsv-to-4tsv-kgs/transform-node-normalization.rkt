#lang racket/base

(require "transform-edge-jsonl.rkt"
         "transform-node-normalization-nodes.rkt")

(transform-node-normalization-nodes)

(transform-edge-jsonl "../../neo-data/raw_downloads_from_kge_archive/node_normalization/edges.jsonl"
                     "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/node_normalization/node_normalization.edge.tsv"
                     "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/node_normalization/node_normalization.edgeprop.tsv")
