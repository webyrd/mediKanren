#lang racket/base

(require "transform-2tsv-to-4tsv-kgs/transform-temporary-kg.rkt"
         "build-mediKanren2-kg-from-4tsv/import-temporary-kg.rkt")



; These variables need your input
; --------------------------------------------------------
; The folder name that the raw data you store in               
(define raw-data-folder-name "NAGPA/")
; The file name of the edge data, inside the folder above
(define nodes-file-name "nodes.jsonl")
; The file name of the node data, inside the folder above
(define edges-file-name "edges.jsonl")
; The database name for your dbKanren data
(define output-database-name "NAGPA")
; --------------------------------------------------------


(displayln "mediKanren Knowledge Graph Import Pipeline")
(displayln "Step 1: Transforming raw data to intermediate format")
(transform-temporary-kg raw-data-folder-name nodes-file-name edges-file-name)
(displayln "Data transformation completed")
(displayln "Step 2: Importing to dbKanren")
(import-temporary-kg output-database-name raw-data-folder-name)
(displayln "Database import completed.")
(displayln (string-append "Path to your database: medikanren/medikanren2/neo/neo-data/" output-database-name ".db"))

