#lang racket
(require json)
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

(define (main)
  (with-context
    (lambda ()
      (let* (
             (idvers (log-thunk (lambda () (fetch-recent-kge-versions)) 'fetch-recent-kge-versions))
             (idvers^ (filter has-dispatch? idvers))
             (kgmetas (log-thunk (lambda () (fetch-recent-kgmeta idvers^)) 'fetch-recent-kgmeta))
             (tasks (log-thunk (lambda () (fetch-tasks-completed)) 'fetch-tasks-completed))
             (kgmetas^ (log-thunk (lambda () ((tasks-incomplete kgid-from-kgmeta ver-from-kgmeta) kgmetas tasks '("completed"))) 'tasks-incomplete))
             (tbis (log-thunk (lambda () (map tbi-from-kgmeta kgmetas^)) 'tbis-tosync kgmetas^ tasks)))
        (for ((tbi tbis))
          (fetch-payload-to-disk tbi)
          (process-tbi (dict-ref (config) 's3path-prefix) tbi)
          (commit-task
            `(idver ,(kge-coord-kgid (task-build-index-kgec tbi)) ,(kge-coord-ver (task-build-index-kgec tbi)))
            "completed"
            (string->jsexpr "{}")))))))

(module+ main
  (main))
