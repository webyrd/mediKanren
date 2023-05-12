;; Custodian, thread, channel experiments

(define CONNECTION_TIMEOUT_SECONDS 10)

;; Beware!  Various Racket time operations, such as syncing on alarm
;; events, uses elapsed CPU time, rather than elapsed real time ("wall
;; clock" time/"walltime").
;;
;; When a job is in a queue waiting to be run, the CPU time doesn't
;; advance, which can lead to apparently bizarre timeout behavior.
;;
;; For Translator timeouts, we should use walltime.

(thread
 (lambda ()
   (printf "@@@ hello from watcher-thread\n")
   (define alarm (alarm-evt (+ (current-inexact-milliseconds)
                               (* CONNECTION_TIMEOUT_SECONDS 1000))))
   (let ((evt (sync (thread-dead-evt handler-thread)
                    alarm)))
     (cond
       (evt
        (cond
          ((eq? (thread-dead-evt handler-thread) evt)
           (printf "@@@ Watcher thread got a thread-dead-evt for handler-thread\n")
           (semaphore-post sem))
          ((eq? alarm evt)
           (printf "!!! Watcher thread got an alarm event after ~s seconds\n"
                   CONNECTION_TIMEOUT_SECONDS)
           (semaphore-post sem))
          (else
           (printf "!!! Watcher thread received unexpected event ~s\n" evt)
           (semaphore-post sem))))
       (else
        (printf "!!! Watcher thread received unexpected #f value\n")
        (semaphore-post sem))))))
