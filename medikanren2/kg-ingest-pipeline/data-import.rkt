#lang racket
(provide
    (all-defined-out))
(require "metadata.rkt")
(require "dispatch.rkt")
(require "dispatch-params.rkt")
(require "cmd-helpers.rkt")

(define (cmd-require-racket adir rfile-rkt)
  `((() () ("bash" "-c" ,(format "cd '~a'; pwd; racket -e '(require \"medikanren2/db/~a\")'" adir rfile-rkt)))))

;;; Ask dbs-available to find out how the db identified itself to database-extend-relations!.
(define (cmd-get-dbnames adir rfile-rkt)
  (define stbash (format "cd '~a'; pwd; racket -e '(require \"medikanren2/db/~a\") (require \"medikanren2/base.rkt\") (dbs-available)'" adir rfile-rkt))
  `(((#:out) () ("bash" "-c" ,stbash)
                ("tail" "-1"))))

(define (dispatch-kgec kgec)
  (printf "about to dispatch-build-impl\n")
  (let-values (((rfile-to-require version-of-dbwrapper git-revision cmds-before)
                (dispatch/validation (kge-coord-kgid kgec) (kge-coord-ver kgec))))
    (define cmds-require (cmd-require-racket (adir-repo-ingest) rfile-to-require))
    (run-cmds (append cmds-before cmds-require))))

