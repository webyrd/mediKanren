#lang racket
(provide
 adir-temp
 with-adir-temp-root
 sha1sum
 dry-run
 run-cmds
 dry-runify)
(require "run-shell-pipelines.rkt")

;;;; begin racket parameters
(define adir-temp 
  (make-parameter 
   'adir-temp-placeholder))

(define (with-adir-temp-root thunk-run)
  (let ((adir (path->string (make-temporary-file "medikanren_~a" 'directory #f))))
    (dynamic-wind
      (lambda ()
        #f)
      (lambda ()
        (parameterize ([adir-temp adir]) (thunk-run)))
      (lambda ()
        #f
        (delete-directory/files adir)
        ))))

(define dry-run
  (make-parameter #t))

(define verbose
  (make-parameter #t))

;;;; end racket parameters

(define (run-cmds cmds)
  (when (or (dry-run) (verbose))
    (displayln cmds))
  (when (not (dry-run))
    (run-pipelines cmds)))
#|  (if (dry-run)
(displayln cmds)
(run-pipelines cmds)))
|#

(define (dry-runify thunk name)
  (lambda args
    (when (or (dry-run) (verbose))
      (displayln (cons name args)))
    (when (not (dry-run))
      (apply thunk args))))
#|
(if (dry-run)
    (displayln (cons name args))
    (apply thunk args))))
|#

(define (sha1sum afile)
  (run-cmds
    `(((#:out) ()
              ("sha1sum" ,afile)
              ("head" "-c40")
              ))))

