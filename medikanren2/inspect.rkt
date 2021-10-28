#lang racket
(require "dbk.rkt")
(provide runi runi* runi*/set)

;; When a subquery runs, increment this number.
(define i-level (make-parameter 0))
;; TODO: instrumenting levels on run^ will be harder

(define-syntax (runi stx)
    (syntax-case stx ()
        [(_ n formals expr)
            #'(begin
                (pretty-write (list 'run n (list 'level (i-level)) (query formals expr)))
                (parameterize ((i-level (+ 1 (i-level))))
                    (run n formals expr)))]
    ))

(define-syntax (runi* stx)
    (syntax-case stx ()
        [(_ formals expr)
            #'(begin
                (pretty-write (list 'run* (list 'level (i-level)) (query formals expr)))
                (parameterize ((i-level (+ 1 (i-level))))
                    (run* formals expr)))]
    ))

(define-syntax (runi*/set stx)
    (syntax-case stx ()
        [(_ formals expr)
            #'(begin
                (pretty-write (list 'run*/set (list 'level (i-level)) (query formals expr)))
                (parameterize ((i-level (+ 1 (i-level))))
                    (run*/set formals expr)))]
    ))


