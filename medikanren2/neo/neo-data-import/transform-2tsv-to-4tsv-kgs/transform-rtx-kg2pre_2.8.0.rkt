#lang racket
(require "transform.rkt")

(transform "./"
           "../build-mediKanren2-kg-from-4tsv/"
           "nodes_kg2.8.0pre_dos_free.tsv"
           "edges_kg2.8.0pre.tsv"
           "rtx-kg2pre_2.8.0")
