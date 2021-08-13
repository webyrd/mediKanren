#lang racket
(require "metadata.rkt")
(require "cmd-helpers.rkt")
(require "kge.rkt")
(require "process-tbi.rkt")
(require "current-source.rkt")
(require "dispatch-params.rkt")
(require "kge-params.rkt")
(require "main-params.rkt")

(define (fetch-recent-tasks) '()) ; TODO: in task-queue.rkt

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
                    thunk))))))))))

(define (main)
  (with-context
    (lambda ()
      (let* (
             (idvers (log-thunk (lambda () (fetch-recent-kge-versions)) 'fetch-recent-kge-versions))
             (idvers^ (filter has-dispatch? idvers))
             (kgmetas (log-thunk (lambda () (fetch-recent-kgmeta idvers^)) 'fetch-recent-kgmeta))
             (tasks (fetch-recent-tasks))
             (tbis (log-thunk (lambda () (tbis-tosync kgmetas tasks)) 'tbis-tosync kgmetas tasks)))
        (for ((tbi tbis))
          (fetch-payload-to-disk tbi)
          (process-tbi (dict-ref (config) 's3path-prefix) tbi))))))

(module+ main
  (main))
