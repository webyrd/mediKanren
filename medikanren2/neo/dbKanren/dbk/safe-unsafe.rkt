#lang racket/base
(provide
  unsafe-fx=
  unsafe-fx<=
  unsafe-fx<
  unsafe-fx+
  unsafe-fx-
  unsafe-fx*
  unsafe-fxand
  unsafe-fxlshift
  unsafe-fxrshift
  unsafe-fxquotient
  unsafe-bytes-length
  unsafe-bytes-ref
  unsafe-bytes-set!
  unsafe-vector*-length
  unsafe-vector*-ref
  unsafe-vector*-set!
  unsafe-fxvector-length
  unsafe-fxvector-ref
  unsafe-fxvector-set!)
(require racket/fixnum)

(define unsafe-list-ref        list-ref)
(define unsafe-car             car)
(define unsafe-cdr             cdr)
(define unsafe-fxmin           fxmin)
(define unsafe-fx=             fx=)
(define unsafe-fx<=            fx<=)
(define unsafe-fx<             fx<)
(define unsafe-fx+             fx+)
(define unsafe-fx-             fx-)
(define unsafe-fx*             fx*)
(define unsafe-fxand           fxand)
(define unsafe-fxlshift        fxlshift)
(define unsafe-fxrshift        fxrshift)
(define unsafe-fxquotient      fxquotient)
(define unsafe-bytes-length    bytes-length)
(define unsafe-bytes-ref       bytes-ref)
(define unsafe-bytes-set!      bytes-set!)
(define unsafe-vector*-length  vector-length)
(define unsafe-vector*-ref     vector-ref)
(define unsafe-vector*-set!    vector-set!)
(define unsafe-fxvector-length fxvector-length)
(define unsafe-fxvector-ref    fxvector-ref)
(define unsafe-fxvector-set!   fxvector-set!)
