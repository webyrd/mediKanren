#lang racket/base
(provide place-main)
(require racket/place)

(define (place-main c.in)
  (displayln "place 2")
  (place-channel-put c.in `(done: ,(cons 'place2 (place-channel-get c.in)))))
