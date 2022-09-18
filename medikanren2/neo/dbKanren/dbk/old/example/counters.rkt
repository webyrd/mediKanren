#lang racket/base
(provide dbk.counters)
(require "base.rkt"

(define-dbk dbk.counters
  (module 'foo
    (<<+ (foo (+ n 1)) (foo n)))
  (module 'bar
    (<<+ (bar (+ n 1)) (bar n)))
  (module 'initialize
    (<<+ (foo 0))
    (<<+ (bar 0)))
  (module 'clean
    (<<- (foo n) (bar n))
    (<<- (bar n) (foo n))))
