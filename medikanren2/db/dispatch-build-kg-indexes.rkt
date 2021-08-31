#lang racket
(provide
 dispatch-build-kg)
(require "../kg-ingest-pipeline/dispatch-params.rkt")

(define (dispatch-build-kg kgekgid version)
  (cond
    ((equal? kgekgid "rtx-kg2")
     `((require-file . "rtx-kg2-20210816.rkt")
       (local-name . "rtx-kg2/20210816")
       (shell-pipeline-before . ())
       (git-revision . "/kg_builder-1")
       (version-of-dbwrapper . "1")))
    ((equal? kgekgid "rtx-kg2-lines1000")
     `((require-file . "rtx-kg2-lines1000-1.0.rkt")
       (local-name . "rtx-kg2-lines1000/1.0")
       (shell-pipeline-before . ())
       (git-revision . "/kg_builder-1")
       (version-of-dbwrapper . "1")))
    ((and (equal? kgekgid "yeast-sri-reference-kg-tsv") (equal? version "1.2"))
     `((require-file . "yeast-sri-reference-kg-1.0.rkt")
       (local-name . "yeast-sri-reference-kg/1.0")
       (shell-pipeline-before . (
        (() () ("bash" ,(format "~a/medikanren2/util/data-import-workaround/trim-incomplete-line.sh" (adir-repo-ingest)) "upstream/yeast-sri-reference-kg-tsv" "nodes.tsv"))
        (() () ("bash" ,(format "~a/medikanren2/util/data-import-workaround/trim-incomplete-line.sh" (adir-repo-ingest)) "upstream/yeast-sri-reference-kg-tsv" "edges.tsv"))))
       (git-revision . "/kg_builder-1")
       (version-of-dbwrapper . "1")))
    (else 'unknown-format)
    ))
