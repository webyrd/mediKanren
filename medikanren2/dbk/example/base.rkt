#lang racket/base
(provide m.base)

(define-dbk m.base
  (<<= (==    u u))
  (<<= (=/=   u v) (not (== u u)))
  (<<= (any<= u v) (== #t (any<= u v)))
  (<<= (any<  u v) (=/= u v) (any<= u v))
  (<<= (any>= u v) (any<= v u))
  (<<= (any>  u v) (any<  v u))

  (relation (member x ys)
    modes ((ys)))  ;; this mode could be inferred
  (<<= (member x (cons x ys)))
  (<<= (member x (cons y ys))
    (=/= x y) (member x ys))
  ;; member can also be defined with a single rule:
  ;(<<= (member x ys)
  ;  (exist (a d)
  ;    (== ys `(,a . ,d))
  ;    (or (== x a)
  ;        (and (=/= x a) (member x d)))))

  (relation (append xs ys xsys)
    modes ((xs) (xsys)))  ;; these modes could be inferred
  (<<= (append '() ys ys))
  (<<= (append `(,x . ,xs) ys `(,x . ,xsys))
    (append xs ys xsys))

  ;; Can this theorem be proven?
  ;(assert (all (xs ys)
  ;          (iff (append xs ys ys)
  ;               (== xs '()))))
  )
