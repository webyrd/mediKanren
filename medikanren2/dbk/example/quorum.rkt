#lang racket/base
(provide m.quorum-vote)
(require "base.rkt")

;; Based on figures 2 and 3 in: Logic and Lattices for Distributed Programming
;; https://dsf.berkeley.edu/papers/socc12-blooml.pdf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define program modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-dbk m.quorum-vote
  (<<+ (vote id) (vote.in id))

  ;; Demonstrate intermediate term definitions
  (define vote-count (set-count (query id (vote id))))
  (define quorum?    (<= quorum-size count))
  (<<~ (result.out 'success) (== #t quorum?))

  ;; This is what it looks like without intermediate definitions
  ;(<<~ (result.out 'success)
  ;  (== #t (<= quorum-size (set-count (query id (vote id))))))

  ;; And this is an alternative where <= is used as a relation instead
  ;(<<~ (result.out 'success)
  ;  (<= quorum-size (set-count (query id (vote id)))))
  )

(define pipe.result (dbk:pipe))

(define program
  (link m.quorum-vote
        ;; Factored out so that m.quorum-vote is reusable
        (dbk
          (parameter quorum-size 3)
          (input     (vote.in    id) _)  ; TODO: fill _ with an actual input device
          (output    (result.out x)  (out:pipe pipe.result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define p (process program))

;; Loop until enough votes arrive (assumes vote.in input device is populated concurrently)
(let loop ()
  (unless (s-member '(success) (pipe-get pipe.result))
    (p 'tick!)
    (loop)))
