#lang racket

; has required package:
;   raco pkg install chk
;
; ways to run include:
;   (cd biolink && raco test -m test/unit)
;   (cd biolink && raco test test/unit/chk-configref.rkt)

(require chk)
(require "../../configref.rkt")

(define (does-throw thunk)
    (with-handlers ([exn:fail?
                   (λ (e) #t)])
    (thunk)
    #f)
)

; test the does-throw function
(chk
    #:t (not (does-throw (lambda () 1))))
(chk
    #:t (does-throw (lambda () (error "an error"))))

; test config-ref
(chk
    #:= (config-ref "foo" #:testing-dict '(("foo" . 1))) 1)
(chk
    #:t (not (does-throw (lambda () (config-ref "foo" #:testing-dict '(("foo" . 1)))))))
(chk
    #:t (does-throw (lambda () (config-ref "foo" #:testing-dict '(("bar" . 1))))))
