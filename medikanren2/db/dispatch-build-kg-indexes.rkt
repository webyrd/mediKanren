#lang racket
(provide
 dispatch-build-kg)

(define (dispatch-build-kg kgekgid version adir-base)
  (cond
    ((equal? kgekgid "rtx") 
     `((require-file . "rtx2-20210204.rkt")
       (shell-pipeline-before . ())))
    ((equal? kgekgid "sri-reference-kg") 
     `((require-file . "sri-reference-kg-0.3.0.rkt")
       (shell-pipeline-before . 
                              (
                               (() ()
                                   ("bash" ,(path->string (simplify-path (build-path adir-base "util/data-import-workaround/remove_cr.sh")))))
                               ))))
    ((equal? kgekgid "yeast-sri-reference-kg-tsv") 
     `((require-file . "yeast-sri-reference-kg-1.0.rkt")
       (local-name . "yeast-sri-reference-kg")
       (shell-pipeline-before . ())))
    (else 'unknown-format)
    ))
