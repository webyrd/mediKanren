#lang racket
(provide
    afile-current-source
    adir-current-source)

(define-syntax (afile-current-source stx)
  (datum->syntax
   (quote-syntax here)
   (syntax-source stx)
   stx))

(define-syntax (adir-current-source stx)
  (datum->syntax
   (quote-syntax here)
    (simplify-path (build-path (syntax-source stx) 'up))
   stx))

