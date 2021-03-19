#lang racket/base
(require
  "csv.rkt"
  racket/list
  )

(define argv (current-command-line-arguments))
(define argv-expected '#(IN_CSV))

(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error 'cmd-line-args (format "expected ~s; given ~s" argv-expected argv)))

(define csv-file-name (vector-ref argv 0))

(define count 0)

(define (validate record)
  (define (concept xs)  ;; CUI, NAME, SEMTYPE, NOVELTY
    (when (not (equal? "C" (substring (car xs) 0 1)))
      (error 'concept (format "unexpected CUI ~s" (car xs)))))
  (define concepts (drop record 4))
  (concept (take concepts 4))  ;; subject
  (concept (drop concepts 4))  ;; object
  (set! count (+ count 1))
  (when (= 0 (remainder count 100000)) (printf "processed ~s rows\n" count)))

(time (call-with-input-file
        (expand-user-path csv-file-name)
        (lambda (in)
          (read-line in 'any)
          ((csv-records validate) in))))
