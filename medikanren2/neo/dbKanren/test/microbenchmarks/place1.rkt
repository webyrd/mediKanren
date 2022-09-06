#lang racket/base
(provide place-main)
(require "shared.rkt" racket/place)

(define (place-main c.in)
  (displayln "place 1")
  (shared-put (+ 1 (shared-get)))
  (place-channel-put c.in `(done: ,(shared-get))))
