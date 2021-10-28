#lang racket
(require "dbk.rkt")
(provide runi runi* runi*/set)

(define-syntax (runi stx)
    (syntax-case stx ()
        [(_ n formals expr)
            #'(begin
                (pretty-write (list 'run n (query formals expr)))
                (run n formals expr))]
    ))

(define-syntax (runi* stx)
    (syntax-case stx ()
        [(_ formals expr)
            #'(begin
                (pretty-write (list 'run* (query formals expr)))
                (run* formals expr))]
    ))

(define-syntax (runi*/set stx)
    (syntax-case stx ()
        [(_ formals expr)
            #'(begin
                (pretty-write (list 'run*/set (query formals expr)))
                (run*/set formals expr))]
    ))


