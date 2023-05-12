;; Custodian, thread, channel experiments

;; This version seems to successfully kill the t1 thread by shutting
;; down the custodian cust, and shut down the t2 thread as well.
;; The done! message is never displayed.

(define *chan* (make-channel))
(define *n* 0)

(printf "*** 1 *n*: ~s\n" *n*)
(define t1-thunk
  (lambda ()
    (printf "t1-thunk invoked\n")
    (let loop ()
      (set! *n* (add1 *n*))
      (loop))))

(printf "*** 2 *n*: ~s\n" *n*)

(define t1
  (let ()
    (define t1-thunk-cust (make-custodian))    
    (parameterize ((current-custodian t1-thunk-cust))
      (thread (lambda ()
                (printf "about to put t1-thunk-cust into channel *chan*\n")
                (channel-put *chan* t1-thunk-cust)
                (printf "done putting t1-thunk-cust into channel *chan*\n")
                (t1-thunk))))))

(define-values (t2 cust)
  (let ()
    (define t2-cust (make-custodian))
    (parameterize ((current-custodian t2-cust))
      (values
        (thread
         (lambda ()
           (printf "*** 3 *n*: ~s\n" *n*)
           (sleep 1)
           (printf "*** 4 *n*: ~s\n" *n*)
           (define cust (channel-get *chan*))
           (printf "*** 5 *n*: ~s\n" *n*)
           (sleep 1)
           (printf "*** 6 *n*: ~s\n" *n*)
           (custodian-shutdown-all cust)
           (printf "*** 7 *n*: ~s\n" *n*)
           (sleep 1)
           (printf "*** 8 *n*: ~s\n" *n*)
           (printf "shutting down t2 cust\n")
           (custodian-shutdown-all t2-cust)
           (printf "done!\n")))
        t2-cust))))
