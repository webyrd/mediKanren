#lang racket
(provide
 dispatch-build-kg)
(require "../kg-ingest-pipeline/dispatch-params.rkt")

(define (dispatch-build-kg kgekgid version)
  (cond
    ((equal? kgekgid "rtx")                 ; currently for illustrative purposes only since currently rtx is not in KGE
     `((require-file . "rtx2-20210204.rkt")
       (shell-pipeline-before . ())))
    ((and (equal? kgekgid "yeast-sri-reference-kg-tsv") (equal? version "1.2"))
     `((require-file . "yeast-sri-reference-kg-1.0.rkt")
       (local-name . "yeast-sri-reference-kg/1.0")
       (shell-pipeline-before . (
        (() () ("bash" ,(format "~a/medikanren2/util/data-import-workaround/trim-incomplete-line.sh" (adir-repo-ingest)) "upstream/yeast-sri-reference-kg-tsv" "nodes.tsv"))
        (() () ("bash" ,(format "~a/medikanren2/util/data-import-workaround/trim-incomplete-line.sh" (adir-repo-ingest)) "upstream/yeast-sri-reference-kg-tsv" "edges.tsv"))
       ))))
    (else 'unknown-format)
    ))
