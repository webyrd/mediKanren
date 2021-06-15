#lang racket
(provide
 run/progress
 )
(require "../../medikanren2/base.rkt"
         chk)


;;;(run/progress numMax s)
;; 
;;   Run a query while reporting statistics.  Query must first be converted to a
;;   stream s.  Parameter numMax indicates the maximum desired number of
;;   query results.  Setting numMax to -1 searches for all results, as in run*.
;;   Statistics are printed to current-output-port.
;; 
;;   Example:
;; 
;;   (run/progress 1000
;;                (query->stream
;;                 (query (id)
;;                        (fresh (p o)
;;                               (nodes id p o)
;;                               (:== #t (id) (string-prefix? id "APO"))
;;                               ))))
;; 
;; Statistics printed are:
;; 
;;   num => the number of results obtained so far
;;   w => the number of ticks expended so far
;;   sec => the number of seconds elapsed so far
;;   num/sec => the results per second found so far
;; 
(define (run/progress numMax s)
  (let ((t0 (current-milliseconds))
        (t-prev -1)
        (dt-min 5000))
    (define (report i w t)
      (let ((dt-prev (- t t-prev)))
        (when (> dt-prev dt-min)
          (let* (
                 (dt (- t t0))
                 (ips (exact->inexact (* 1000 (/ i (+ dt 1)))))
                 (dtsec (exact->inexact (/ dt 1000)))
                 )
            (printf "num=~a w=~a sec=~a num/sec=~a\n" i w dtsec ips)
            (set! t-prev t)))))
    (define (iter i w xs s)
      (cond
        ((and (>= numMax 0) (>= i numMax)) xs)
        ((null? s) xs)
        ((procedure? s)
         (report i w (if (> (bitwise-and w 255) 0) 0 (current-milliseconds)))
         (iter i (+ w 1) xs (s)))
        ((pair? s)
         (let* ((x (car s))
                (t (current-milliseconds))
                )
           (report i w t)
           (iter (+ i 1) (+ w 1) (cons x xs) ((cdr s)))))
        (else (raise (format "unexpected type: ~a" s)))))
    (reverse (iter 0 0 '() s))))

(module+ test
  (chk
   (#:do
    (define-relation/table (rel a b)
      'source-stream '(
                       ("a" 1)
                       ("b" 2)
                       ("c" 3)
                       ("d" 4)
                       ("e" 5))))

   (#:= (run
         10
         (num)
         (fresh (id)
                (membero id '("b" "c"))
                (rel id num)))
    '((2) (3)))
   
   (#:= (run/progress
         10
         (query->stream
          (query             
           (num)
           (fresh (id)
                  (membero id '("b" "c"))
                  (rel id num)))))
    '((2) (3)))
  ))
