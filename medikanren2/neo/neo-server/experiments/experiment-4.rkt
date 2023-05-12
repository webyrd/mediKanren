;; Custodian, thread, channel experiments

;; This version seems to successfully kill the t1 thread by shutting
;; down the custodian cust

(define *n* 0)

(printf "*** 1 *n*: ~s\n" *n*)
(define t1-thunk
  (lambda ()
    (let loop ()
      (set! *n* (add1 *n*))
      (loop))))

(printf "*** 2 *n*: ~s\n" *n*)
(define-values (t1 cust)
  (let ()
    (define t1-thunk-cust (make-custodian))
    (parameterize ((current-custodian t1-thunk-cust))
      (values (thread t1-thunk) t1-thunk-cust))))

(printf "*** 3 *n*: ~s\n" *n*)
(sleep 1)
(printf "*** 4 *n*: ~s\n" *n*)
(custodian-shutdown-all cust)
(printf "*** 5 *n*: ~s\n" *n*)
(sleep 1)
(printf "*** 6 *n*: ~s\n" *n*)
