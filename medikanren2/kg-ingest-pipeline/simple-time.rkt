#lang racket
(require racket/date)
(require chk)
(provide
    format-yyyy-from-tsec
    )

; Could have used https://docs.racket-lang.org/gregor/time-format.html, but that
; seemed a bit elaborate for the current scope.

(define (format-yyyy-from-tsec tsec)
    (define d (seconds->date tsec #f))
    (string-append
        (~a (date-year d)
            #:align 'right
            #:width 4
            #:pad-string "0")
        (~a (date-month d)
            #:align 'right
            #:width 2
            #:pad-string "0")
        (~a (date-day d)
            #:align 'right
            #:width 2
            #:pad-string "0")
        (~a (date-hour d)
            #:align 'right
            #:width 2
            #:pad-string "0")
        (~a (date-minute d)
            #:align 'right
            #:width 2
            #:pad-string "0")
        (~a (date-second d)
            #:align 'right
            #:width 2
            #:pad-string "0")))

(define (parse-tsec-from-yyyy st)
    (find-seconds
        (string->number (substring st 12 14))
        (string->number (substring st 10 12))
        (string->number (substring st 8 10))
        (string->number (substring st 6 8))
        (string->number (substring st 4 6))
        (string->number (substring st 0 4))
        #f
        ))

(chk
    (#:do (define t (floor (/ (current-milliseconds) 1000))))
    (#:= (parse-tsec-from-yyyy (format-yyyy-from-tsec t))
        t))

        