#lang racket
;(require racket)
;(require racket/match)
(require chk)
(require "../../common.rkt")
(require "../../profiling.rkt")
(require "../../string-search.rkt")
(require racket/pretty)



  
  
;; We count both work ticks (w) and results (i).  Make a version
;; parametreized by numTicksMax?
(define (run-stream numMax s)
  (define (iter i w xs s)
    (cond
      ((and (>= numMax 0) (>= i numMax)) xs)
      ((null? s) xs)
      ((procedure? s)
       (iter i (+ w 1) xs (s)))
      ((pair? s)
       (let* ((x (car s))
              (t (current-milliseconds))
              )
         (iter (+ i 1) (+ w 1) (cons x xs) ((cdr s)))))
      (else (raise (format "unexpected type: ~a" s)))))
  (reverse (iter 0 0 '() s)))


(define (run-query numMax q)
  (run-stream numMax (query->stream q)))


(define uri-ion-transport "GO:0034765")

;; Can we find one particular name?
(chk
 (#:=
  (run-query 10 (query-names-from-id-rel uri-ion-transport cprop))
  '(("regulation of ion transmembrane transport"))))


;; Can we find 10 names without crashing?
(chk
 (#:=
  (length (run-query 10
            (query-names-from-rel cprop)))
  10))


