#lang racket/base
(require "dbk/io.rkt")

(module+ main
  (let ((in (current-input-port)) (out (current-output-port)))
    (with-handlers (((lambda (e)
                       (and (exn:fail:filesystem:errno? e)
                            (equal? (exn:fail:filesystem:errno-errno e)
                                    '(32 . posix))))
                     void))
      (let loop ()
        (let ((row (csv:read in)))
          (unless (eof-object? row)
            (tsv:write out row)
            (loop)))))))
