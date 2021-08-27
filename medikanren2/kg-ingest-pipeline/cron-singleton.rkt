#lang racket
(provide
    (all-defined-out))

(define (afile-lock-script-singleton)
    (define afile-script (find-system-path 'run-file))
    (define adir (find-system-path 'temp-dir))
    (define patel-script (last (explode-path afile-script)))
    (define patel (string->path-element (format "~a.lock" (path-element->string patel-script))))
    (build-path adir patel))

;;; Go to sleep if the current script is already running.
(define (try-lock-singleton thunk)
    (define afile (afile-lock-script-singleton))
    (call-with-file-lock/timeout
        (find-system-path 'temp-dir)  ; strange that this has to be empty if it is not used
        'exclusive
        thunk
        (lambda () 
            ;(printf "Already running, good night.\n")
            "")
        #:lock-file afile))
