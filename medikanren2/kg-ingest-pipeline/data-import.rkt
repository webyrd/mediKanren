#lang racket
(provide
    (all-defined-out))
(require "metadata.rkt")
(require "dispatch.rkt")
(require "dispatch-params.rkt")
(require "cmd-helpers.rkt")

(define (cmd-require-racket adir rfile-rkt hsig)
    ; We generate a temporary racket file so that we can get full stack traces on errors.
    ; If we use racket "-e" then "-l errortrace" will not work at the same time.  To make
    ; things even more fun, racket will reject a require statement containing an absolute
    ; path.
  (define afile-bash (path->string (build-path adir "medikanren2" "kg-ingest-pipeline" "write-require-racket.sh")))
  (define afile-gen-rkt (path->string (build-path adir hsig "ingest.rkt")))
  (define afile-rkt (format "../medikanren2/db/~a" rfile-rkt))
  (printf "afile-bash=~a afile-gen-rkt=~a afile-rkt=~a\n" afile-bash afile-gen-rkt afile-rkt)
  (unless (dry-run)
    (make-parent-directory* afile-gen-rkt))
  `((() () ("bash" ,afile-bash ,afile-rkt ,afile-gen-rkt))
      (() () ("bash" "-c" ,(format "cd '~a'; pwd; racket -l errortrace -u ~a" adir afile-gen-rkt)))))

;;; Ask dbs-available to find out how the db identified itself to database-extend-relations!.
(define (cmd-get-dbnames adir rfile-rkt)
  (define stbash (format "cd '~a'; pwd; racket -e '(require \"medikanren2/db/~a\") (require \"medikanren2/base.rkt\") (dbs-available)'" adir rfile-rkt))
  `(((#:out) () ("bash" "-c" ,stbash)
                ("tail" "-1"))))

(define (dispatch-kgec kgec hsig)
  (printf "about to dispatch-build-impl\n")
  (let-values (((rfile-to-require version-of-dbwrapper git-revision cmds-before)
                (dispatch/validation (kge-coord-kgid kgec) (kge-coord-ver kgec))))
    (define cmds-require (cmd-require-racket (adir-repo-ingest) rfile-to-require hsig))
    (run-cmds (append cmds-before cmds-require))))

