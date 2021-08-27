#lang racket
(provide
 afile-current-source
 adir-current-source)

(define (afile-current-source)
  (define p (find-system-path 'run-file))
  (if (absolute-path? p)
      (simplify-path p)
      (simplify-path
       (build-path (current-directory) p))))

(define (adir-current-source)
  (simplify-path (build-path (afile-current-source) 'up)))

