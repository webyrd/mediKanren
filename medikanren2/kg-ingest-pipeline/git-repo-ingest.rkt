#lang racket
(provide
    (all-defined-out))
(require "dispatch-params.rkt")
(require "current-source.rkt")
(require "pipesig.rkt")
(require "../../stuff/cmd-helpers.rkt")

#|
    Make sure we have the latest or otherwise most appropriate version to build.
|#

(define (with-adir-repo-ingest adir-repo thunk)
  (parameterize ((adir-repo-ingest (simplify-path (build-path adir-repo 'up "mediKanren-ingest"))))
    (thunk)))

(define (cmds-repo-ingest-checkout-version remote revision)
    `((() ()
        ("bash" "-c" ,(format "cd \"~a\" && git fetch ~a && git checkout ~a" (adir-repo-ingest) remote revision))))
    ; TODO: do shell-pipelines preserve current directory between commands?
)

(define (prepare-git-repo-ingest psig)
    (define remote "origin")
    (define revision (psig-extra-ref psig "git-revision"))
    (define revision-fq
        (if (equal? (substring revision 0 1) "/")
            (format "~a~a" remote revision)
            revision))
    (run-cmds
        (cmds-repo-ingest-checkout-version remote revision-fq))
    ; TODO: return commit number
)
