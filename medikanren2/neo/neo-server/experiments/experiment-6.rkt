;; Custodian, thread, channel experiments

;; This version seems nice!

#|
Welcome to Racket v8.6 [cs].
> (load "experiment-6.rkt")
*** 1 *n*: 0
*** 2 *n*: 0
about to put t1-thunk-cust into channel *chan*
about to sleep
*** 3 *n*: 0
*** 4 *n*: 0
*** 5 *n*: 0
done putting t1-thunk-cust into channel *chan*
t1-thunk invoked
*** 6 *n*: 201695745
*** 7 *n*: 201695745
*** 8 *n*: 201695745
*** 9 (looping) *n*: 201695745
*** 9 (looping) *n*: 201695745
*** 9 (looping) *n*: 201695745
shutting down t2 cust
done!
> 
|#

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
           (let loop ()
             (printf "*** 9 (looping) *n*: ~s\n" *n*)
             (sleep 1)
             (loop))))
        t2-cust))))

(printf "about to sleep\n")
(sleep 6)
(printf "shutting down t2 cust\n")
(custodian-shutdown-all cust)
(printf "done!\n")
