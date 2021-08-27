#lang racket
(provide
    (all-defined-out))

;(require "dispatch-params.rkt")
(require "cmd-helpers.rkt")
(require "../db/dispatch-build-kg-indexes.rkt")
(require "../../stuff/run-shell-pipelines.rkt")

(define (has-dispatch? idver)
  (match idver
    (`(idver ,kgid ,ver)
     (list? (dispatch-build-kg kgid ver)))))

(define ((kg-ref key (val-default 'kg-ref-default)) kgid ver)
  (define kg (dispatch-build-kg kgid ver))
  (if (dict-has-key? kg key)
      (dict-ref kg key)
      (begin
        (unless (not (equal? val-default 'kg-ref-default))
          (error (format "dispatch-build-kg-indexes key ~a is required for kgid=~a version=~a" key kgid ver)))
        val-default)))

(define require-file-from-kg (kg-ref 'require-file))
(define shell-pipeline-before (kg-ref 'shell-pipeline-before '()))
(define local-name-from-kg (kg-ref 'local-name))
(define version-of-dbwrapper-from-kg (kg-ref 'version-of-dbwrapper))
(define git-revision (kg-ref 'git-revision "/master"))

(define (dispatch/validation kgid ver)
  ; dispatch/validation is called twice, once on startup and again
  ; once data is available to be processed.  Make sure that all
  ; required kg-ref arguments are fetched here so that absent or
  ; invalid arguments in the dispatch rules fail fast:
  (local-name-from-kg kgid ver)
  ; <insert any other required kg-ref arguments here>
  ;
  ; TODO: git pull adir-repo-ingest, optionally pinning
  ; a version from dispatch-build-kg-indexes.rkt.
  ; TODO: copy file_set.yaml, provider.yaml
  (let ((cmds-before (shell-pipeline-before kgid ver)))
    (begin
      (report-invalid-pipelines cmds-before)
      (values
        (require-file-from-kg kgid ver)
        (version-of-dbwrapper-from-kg kgid ver)
        (git-revision kgid ver)
        cmds-before))))

(define (version-of-dbwrapper/validation kgid ver)
  (let-values (((rfile-to-require version-of-dbwrapper git-revision cmds-before)
                (dispatch/validation kgid ver)))
    (values version-of-dbwrapper git-revision)))