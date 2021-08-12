#lang racket
(provide
 dispatch-build-kg)
(require "../kg-ingest-pipeline/dispatch-params.rkt")

(define (dispatch-build-kg kgekgid version)
  (cond
    ((equal? kgekgid "rtx") 
     `((require-file . "rtx2-20210204.rkt")
       (shell-pipeline-before . ())))
    ((equal? kgekgid "sri-reference-kg")
     `((require-file . "sri-reference-kg-0.3.0.rkt")
       (shell-pipeline-before . 
                              (
                               (() ()
                                   ("bash" ,(path->string (simplify-path (build-path (adir-repo-ingest) "medikanren2/util/data-import-workaround/remove_cr.sh")))))
                               ))))
    ((and (equal? kgekgid "yeast-sri-reference-kg-tsv") (string>=? version "1.2"))
     `((require-file . "yeast-sri-reference-kg-1.0.rkt")
       (local-name . "yeast-sri-reference-kg")
       (shell-pipeline-before . ())))
    (else 'unknown-format)
    ))
