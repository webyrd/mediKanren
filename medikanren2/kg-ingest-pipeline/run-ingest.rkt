#lang racket
(require json)
(require aws)
(require "metadata.rkt")
(require "cmd-helpers.rkt")
(require "kge.rkt")
(require "process-tbi.rkt")
(require "current-source.rkt")
(require "dispatch-params.rkt")
(require "dispatch.rkt")
(require "kge-params.rkt")
(require "main-params.rkt")
(require "task-checklist.rkt")
(require "sideload-helpers.rkt")
(require "pipesig.rkt")

(define (with-context thunk)
  (with-config
    (lambda ()
      (with-adir-temp-root
        (lambda ()
          (parameterize ((dry-run #f))
            (let ((adir-repo (simplify-path (build-path (adir-current-source) 'up 'up))))
              (with-adir-repo-ingest adir-repo
                (lambda ()
                  (with-kge-token
                    (lambda ()
                      (thunk))))))))))))

(define states-resolved '("completed" "failed"))

(define (mark-task psig state ex msg)
  (when msg
    (displayln msg))
  (when ex
    (displayln ex))
  (commit-task
    psig
    state))

(define (main)
  (with-context
    (lambda ()
      (let* (
             (kgmetas (fetch-kgmetas-kge))
             (psigs-kge (map psig-from-kgmeta kgmetas))
             (tasks-out (log-thunk (lambda () (fetch-task-events)) 'fetch-task-events))
                ; Fetch s3 paths.  We need them to figure out what has already been built.  Because there
                ; are no two step transformations, we didn't need them to figure out what we could build.
             (psigs-kge^ (log-thunk (lambda () (tasks-unresolved psigs-kge tasks-out state-from-check hsig-from-check states-resolved)) 'tasks-unresolved))
                ; Figure out incomplete transformations.
             (psigs-sideload (map psig-from-sideload (fetch-sideload-events)))
             (psigs-sideload^ (log-thunk (lambda () (tasks-unresolved psigs-sideload tasks-out state-from-check hsig-from-check states-resolved)) 'tasks-unresolved)))
        (for ((psig psigs-sideload^))
          (define tbi (tbi-from-sideload-psig psig))
          (printf "\n\nfetching sideload psig=~s\n" psig)
          (sideload-fetch-to-disk psig)
          (with-handlers
            (
              [exn:fail:aws?
                (lambda (ex)
                  (mark-task psig "fault" ex "kg-ingest-pipeline failed with exception that may be transient.  Bypassing commit so that job will retry"))]
              [exn:fail?
                (lambda (ex)
                  (mark-task psig "failed" ex "kg-ingest-pipeline failed in local processing, which is likely to be a deterministic failure due to bad configuration.  To retry, change configuration."))])
            (begin
              (process-tbi (s3path-base) psig tbi)
              (mark-task psig "completed" #f #f))))
        (for ((psig psigs-kge^))
          (define tbi (tbi-from-kgmeta (psig-extra-ref psig "kgmeta")))
          (printf "\n\nfetching tbi from psig=~s\n" psig)
          (fetch-payload-to-disk tbi)
          (with-handlers
            (
              [exn:fail:aws?
                (lambda (ex)
                  (mark-task psig "fault" ex "kg-ingest-pipeline failed with exception that may be transient.  Bypassing commit so that job will retry"))]
              [exn:fail?
                (lambda (ex)
                  (mark-task psig "failed" ex "kg-ingest-pipeline failed in local processing, which is likely to be a deterministic failure due to bad configuration.  To retry, change configuration."))])
            (begin
              (process-tbi (s3path-base) psig tbi)
              (mark-task psig "completed" #f #f))))
      ))))

(module+ main
  (main))
