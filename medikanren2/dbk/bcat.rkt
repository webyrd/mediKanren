#lang racket/base
(require "dbk/codec.rkt" racket/pretty racket/port)

(module+ main
  (define argv      (current-command-line-arguments))
  (define arg-names '#(DATA-TYPE))
  (unless (= (vector-length argv) (vector-length arg-names))
    (error "invalid arguments" 'expected arg-names 'given argv))
  (define type (with-input-from-string (vector-ref argv 0) read))
  (define in   (current-input-port))
  (define out  (current-output-port))
  (with-handlers (((lambda (e)
                     (and (exn:fail:filesystem:errno? e)
                          (equal? (exn:fail:filesystem:errno-errno e)
                                  '(32 . posix))))
                   void))
    (let loop ()
      (unless (eof-object? (peek-byte in))
        (pretty-write (decode in type))
        (loop)))))