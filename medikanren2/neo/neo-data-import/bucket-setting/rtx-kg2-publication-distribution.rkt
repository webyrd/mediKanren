#lang racket

(require "predicate-publication-statistics.rkt"
         "../transform-2tsv-to-4tsv-kgs/transform-utils.rkt")

(define KG_DIRECTORY "../../neo-data/raw_downloads_from_kge_archive/rtx-kg2-v2.10.0/")
(define KG_EDGE_File "data_01_RAW_KGs_rtx_kg2_v2.10.0_validated_rtx-kg2_2.10.0_edges.tsv")
(define KG_PATH (string-append KG_DIRECTORY KG_EDGE_File))
(define FILE_TYPE "tsv")
(define SCORE_FACTOR #f)

(define pub-distribution
  (cond
    ((equal? FILE_TYPE "jsonl") (get-pub-dist-jsonl KG_PATH))
    ((equal? FILE_TYPE "tsv") (get-pub-dist-tsv KG_PATH SCORE_FACTOR))
    (else (error "mediKanren does not support the file type ~a." FILE_TYPE))))

(define formated-pub-distribution
  (unwrap (map (lambda (p)
                 (map
                  (lambda (pub/count)
                    (list (car p) (car pub/count) (cdr pub/count)))
                  (hash->list (cdr p))))
               pub-distribution)))

(write-list-to-tsv
 '("predicate"
   "score"
   "num-edges")
 formated-pub-distribution
 (string-append KG_DIRECTORY "scores.tsv"))