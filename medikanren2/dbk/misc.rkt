#lang racket/base
(provide foldl/and let*/and)

(define (foldl/and f acc xs . yss)
  (let loop ((acc acc) (xs xs) (yss yss))
    (if (null? xs)
      acc
      (and acc (loop (apply f (car xs) (append (map car yss) (list acc)))
                     (cdr xs)
                     (map cdr yss))))))

(define-syntax let*/and
  (syntax-rules ()
    ((_ () body ...)                   (let () body ...))
    ((_ ((lhs rhs) rest ...) body ...) (let ((lhs rhs))
                                         (and lhs (let*/and (rest ...)
                                                    body ...))))))
