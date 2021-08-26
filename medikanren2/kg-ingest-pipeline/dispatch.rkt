#lang racket
(provide
    (all-defined-out))
(require "../db/dispatch-build-kg-indexes.rkt")

(define (has-dispatch? idver)
  (match idver
    (`(idver ,kgid ,ver)
     (list? (dispatch-build-kg kgid ver)))))

(define ((kg-ref key (val-default 'kg-ref-default)) kgid ver)
  (define kg (dispatch-build-kg kgid ver))
  (if (dict-has-key? kg key)
      (dict-ref kg key)
      (begin
        (unless (not (equal? val-default 'kg-ref-default))
          (error (format "dispatch-build-kg-indexes key ~a is required for kgid=~a version=~a" key kgid ver)))
        val-default)))

(define require-file-from-kg (kg-ref 'require-file))
(define shell-pipeline-before (kg-ref 'shell-pipeline-before '()))
(define local-name-from-kg (kg-ref 'local-name))
