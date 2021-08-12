#lang racket
(require "metadata.rkt")
(require "cmd-helpers.rkt")
(require "kge.rkt")
(require "process-tbi.rkt")

(define (fetch-recent-tasks) '()) ; TODO: in task-queue.rkt

(define (with-context thunk)
  (with-config
    (lambda ()
      (with-adir-temp-root
        (lambda ()
          (parameterize ((dry-run #f))
            (with-kge-token
              thunk)))))))

(define (main)
  (with-context
    (lambda ()
      (let* (
             (idvers (log-thunk (lambda () (fetch-recent-kge-versions)) 'fetch-recent-kge-versions))
             (kgmetas (log-thunk (lambda () (fetch-recent-kgmeta idvers)) 'fetch-recent-kgmeta))
             (tasks (fetch-recent-tasks))
             (tbis (log-thunk (lambda () (tbis-tosync kgmetas tasks)) 'tbis-tosync kgmetas tasks)))
        (for ((tbi tbis))
          (when (has-dispatch? tbi)
            (fetch-payload-to-disk tbi)
            (process-tbi (s3path-base) tbi)))))))

(module+ main
  (main))
