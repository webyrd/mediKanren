#lang racket/base
;(provide foo bar)

(require "concrete-syntax-extended.rkt")

(define-relations
  (rule  (foo x)      (== x 5) (bar x 1 2))
  (table (bar id s o) "somewhere/bar"))

((relation-method foo) 'apply 888)
bar

;(define-relation (foo x) (== x 5) (bar x 1 2))

;(define-relation/table (bar id s o) "somewhere/bar")

;(with-formula-vocabulary (foo 11))

;(foo 'apply 12)

;(== 1 2)
;(with-formula-vocabulary (== 1 2))
;==
;;)
