#lang racket
(provide
  lognew-info lognew-error requestid)
(require
  racket/date
  json
)
(define requestid (make-parameter -1))

(date-display-format 'iso-8601)

(define (lognew-message level msg)
  (define t (current-seconds))
  (define st-t (date->string (seconds->date t #f) #t))
  (define jsexpr
    (if (hash? msg)
      (hash-set
        (hash-set
          (hash-set msg
            'level (symbol->string level))
          'requestid (requestid))
        't st-t)
      (hasheq
        'msg msg
        't st-t
        'requestid (requestid)
        'level (symbol->string level))))
  (with-handlers ([exn:fail?
                   (λ (e)
                   #|
                      Racket json is very strict about what it accepts as a jsexpr.  For example, this succeeds:
                        (jsexpr->string (hasheq 'foo "bar"))

                      but these both consider their input to be ill-formed and throw:
                        (jsexpr->string (hasheq "foo" "bar"))

                        (jsexpr->string (hasheq 'foo '#"bar"))
                   |#
                     (printf
                      "Caught exception in lognew-message when converting/printing jsexpr->string.\nlevel:\n~s\nmsg:\n~s\njsexpr:\n~s\nexception:\n~s\n"
                      level
                      msg
                      jsexpr
                      e))])
    (displayln
     (jsexpr->string jsexpr)))
  (flush-output (current-output-port)))

(define (lognew-info msg)
  (lognew-message 'info msg))

(define (lognew-error msg)
  (lognew-message 'error msg))

