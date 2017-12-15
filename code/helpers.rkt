#lang racket/base
(provide
  membero
  not-membero
  )

(require
  "mk.rkt"
  )



;; list membership
(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        [(== x y)]
        [(=/= x y) (membero x rest)]))))

;; list non-membership
(define not-membero
  (lambda (x ls)
    (conde
      [(== '() ls)]
      [(fresh (y rest)
         (== `(,y . ,rest) ls)
         (=/= x y)
         (not-membero x rest))])))
