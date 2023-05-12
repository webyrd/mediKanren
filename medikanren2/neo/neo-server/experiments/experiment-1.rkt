;; Custodian, thread, channel experiments

;; This version does successfully kill thread t1 by calling
;; kill-thread

(define n 0)

(printf "*** 1 n: ~s\n" n)
(define t1-thunk
  (lambda ()
    (let loop ()
      (set! n (add1 n))
      (loop))))

(printf "*** 2 n: ~s\n" n)
(define t1
  (thread t1-thunk))

(printf "*** 3 n: ~s\n" n)
(sleep 1)
(printf "*** 4 n: ~s\n" n)
(kill-thread t1)
(printf "*** 5 n: ~s\n" n)
(sleep 1)
(printf "*** 6 n: ~s\n" n)
