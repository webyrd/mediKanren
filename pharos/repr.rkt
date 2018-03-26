#lang racket
(provide
  integer->repr
  repr->integer
  decimal->repr
  repr->decimal
  string->repr
  repr->string
  data->repr
  repr->data
  write-repr
  )

(define (integer->repr i) i)
(define (repr->integer i) i)
(define (decimal->repr d) d)
(define (repr->decimal d) d)
(define (string->repr str) str)
(define (repr->string str) str)
(define (data->repr d) (list->vector d))
(define (repr->data v) (vector->list v))
(define (write-repr repr out) (fprintf out "~s\n" repr))

;(define (integer->enc i) (integer->integer-bytes (or i 0) 4 #t #f))
;(define (enc->integer bs) (integer-bytes->integer bs #t #f 0 4))
;(define (decimal->enc d) (real->floating-point-bytes (or d 0.0) 4 #f))
;(define (enc->decimal bs) (floating-point-bytes->real bs #f 0 4))
;(define (string->enc str)
  ;(define bs (string->bytes/utf-8 str))
  ;(bytes-append (integer->integer-bytes (bytes-length bs) 2 #f #f) bs))
;(define (enc->string bs)
  ;(define len (integer-bytes->integer bs #f #f 0 2))
  ;(bytes->string/utf-8 bs #f 2 (+ 2 len)))
;(define (data->enc d) (bytes-append* d))
;(define (enc->data bs) bs)
;(define (write-repr repr out) (write-bytes repr out))
