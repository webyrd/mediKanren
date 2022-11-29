#lang racket/base

(provide
 iota
 pretty-print-json-string
 take-at-most)

(define (iota n)
  (define (iter i)
    (if (>= i n)
        '()
        (cons i (iter (+ 1 i)))))
  (iter 0))

(define (pretty-print-json-string json-string port)
  (define len (string-length json-string))
  (define (display-indent-spaces n port)
    (let loop ([i 0])
      (cond
        [(< i n)
         (display #\space port)
         (loop (add1 i))]
        [else (void)])))
  (let loop ([i 0]
             [indent 0]
             [in-quote #f])
    (cond
      [(< i len)
       (let ((c (string-ref json-string i)))
         (case c
           [(#\")
            (display c port)
            (loop (add1 i) indent (not in-quote))]
           ;;
           [(#\:)
            (display c port)
            (unless in-quote
              (display #\space port))
            (loop (add1 i) indent in-quote)]
           ;;
           [(#\,)
            (display c port)
            (unless in-quote
              (newline port)
              (display-indent-spaces indent port))
            (loop (add1 i) indent in-quote)]
           ;;
           [(#\{ #\[)
            (display c port)
            (if in-quote
                (loop (add1 i) indent in-quote)                
                (let ((indent (add1 indent)))
                  (newline port)
                  (display-indent-spaces indent port)
                  (loop (add1 i) indent in-quote)))]
           ;;
           [(#\} #\])
            (if in-quote
                (begin
                  (display c port)
                  (loop (add1 i) indent in-quote))
                (let ((indent (sub1 indent)))
                  (newline port)
                  (display-indent-spaces indent port)
                  (display c port)
                  (loop (add1 i) indent in-quote)))]
           ;;
           [else
            (display c port)
            (loop (add1 i) indent in-quote)]))]
      [else (void)])))

(define (take-at-most ls n)
  (cond
    [(<= n 0) '()]
    [(null? ls) '()]
    [else
     (cons (car ls)
           (take-at-most (cdr ls) (sub1 n)))]))