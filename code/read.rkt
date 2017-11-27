#lang racket/base
(provide
  read-all
  read-all/stream
  read-all-from-file
  )

(require
  racket/stream
  )

(define (read-all in)
  (define datum (read in))
  (if (eof-object? datum) '() (cons datum (read-all in))))

(define (read-all/stream in)
  (define datum (read in))
  (if (eof-object? datum) '() (stream-cons datum (read-all in))))

(define (read-all-from-file path)
  (call-with-input-file (expand-user-path path) read-all))
