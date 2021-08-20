#lang racket
(require json)
(require aws)
(require "metadata.rkt")
(require "cmd-helpers.rkt")
(require "kge.rkt")
(require "process-tbi.rkt")
(require "current-source.rkt")
(require "dispatch-params.rkt")
(require "kge-params.rkt")
(require "main-params.rkt")
(require "task-checklist.rkt")

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
                      (parameterize ((s3path-base (dict-ref (config) 's3path-prefix)))
                        (thunk)))))))))))))

(define states-resolved '("completed" "failed"))

(define (mark-task tbi state ex msg)
  (when msg
    (displayln msg))
  (when ex
    (displayln ex))
  (let ((jsexpr (string->jsexpr "{}")))
    (commit-task
      `(idver ,(kge-coord-kgid (task-build-index-kgec tbi)) ,(kge-coord-ver (task-build-index-kgec tbi)))
      state
      jsexpr)))

(define (main)
  (with-context
    (lambda ()
      (let* (
             (idvers (log-thunk (lambda () (fetch-kge-catalog)) 'fetch-kge-catalog))
             (idvers^ (filter has-dispatch? idvers))
             (kgmetas (log-thunk (lambda () (fetch-kge-recent-versions idvers^)) 'fetch-kge-recent-versions))
             (tasks (log-thunk (lambda () (fetch-task-events)) 'fetch-task-events))
             (kgmetas^ (log-thunk (lambda () ((tasks-unresolved kgid-from-kgmeta ver-from-kgmeta) kgmetas tasks states-resolved)) 'tasks-unresolved))
             (tbis (log-thunk (lambda () (map tbi-from-kgmeta kgmetas^)) 'tbis-tosync kgmetas^ tasks)))
        (for ((tbi tbis))
          (fetch-payload-to-disk tbi)
          (with-handlers
            (
              [exn:fail:aws?
                (lambda (ex)
                  (mark-task tbi "fault" ex "kg-ingest-pipeline failed with exception that may be transient.  Bypassing commit so that job will retry"))]
              [exn:fail?
                (lambda (ex)
                  (mark-task tbi "failed" ex "kg-ingest-pipeline failed in local processing, which is likely to be a deterministic failure due to bad configuration.  To retry, change configuration."))])
            (begin
              (process-tbi (dict-ref (config) 's3path-prefix) tbi)
              (mark-task tbi "completed" #f #f))))))))

(module+ main
  (main))
