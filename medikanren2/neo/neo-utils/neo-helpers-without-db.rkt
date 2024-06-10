#lang racket/base

(require
 racket/set
 racket/unsafe/ops
 racket/math)

(provide
 maybe-time
 iota
 pretty-print-json-string
 take-at-most
 ;;
 bytes<=?
 set-fixed-point
 unsafe-bytes-split-tab
 bytes-base10->fxnat
 ;;
 minus-one-before-zero
 auto-grow)

;; Use the second definition of 'maybe-time' to see the time use for
;; low-level query calls.
(define maybe-time (lambda (x) x)) ;; no-op
;; (define maybe-time time)


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
  (if (<= (length ls) n)
      ls
      (let loop ((ls ls)
                 (n n))
        (cond
          [(<= n 0) '()]
          [(null? ls) '()]
          [else
           (cons (car ls)
                 (loop (cdr ls) (sub1 n)))]))))

;; copied from 'database.rkt'
(define (bytes<=? a b) (not (bytes<? b a)))

;; copied from 'database.rkt'
(define (set-fixed-point xs.initial step)
  (let loop ((current (set))
             (next    xs.initial))
    (let ((new (set-subtract next current)))
      (if (set-empty? new)
        current
        (loop (set-union current new)
              (step      new))))))

(define (unsafe-bytes-split-tab bs)
  (let loop ((end    (unsafe-bytes-length bs))
             (i      (unsafe-fx- (unsafe-bytes-length bs) 1))
             (fields '()))
    (cond ((unsafe-fx< i 0)                       (cons (subbytes bs 0 end) fields))
          ((unsafe-fx= (unsafe-bytes-ref bs i) 9) (loop i   (unsafe-fx- i 1) (cons (subbytes bs (+ i 1) end) fields)))
          (else                                   (loop end (unsafe-fx- i 1) fields)))))

(define (bytes-base10->fxnat bs)
  (define len (bytes-length bs))
  (unless (< 0 len 19)
    (when (= len 0)  (error "natural number must contain at least one digit" bs))
    (when (< 18 len) (error "natural number must contain at most 18 digits (to safely fit in a fixnum)" bs)))
  (let loop ((i 0) (n 0))
    (if (unsafe-fx< i len)
      (let ((b (unsafe-bytes-ref bs i)))
        (unless (unsafe-fx<= 48 b 57)
          (error "natural number must contain only base10 digits" bs))
        (loop (unsafe-fx+ i 1)
              (unsafe-fx+ (unsafe-fx* n 10)
                          (unsafe-fx- b 48))))
      n)))

(define minus-one-before-zero
  (lambda (n*)
    (and n*
        (if (eq? (car n*) 0)
            #f
            (list (- (car n*) 1))))))

(define (auto-grow hop-proc score* result_amount)
  (let ((half-result (exact-round (/ result_amount 2.0))))
    (let loop ((r '()) (sl score*))
      (cond
        [(> (length r) half-result)
         (printf "return ~a answers\n" (length r))
         r]
        [(andmap not sl)
         (printf "return ~a answers\n" (length r))
         r]
        [else
         #;(printf "number of answers: ~a, take next round\n" (length r))
         (loop (append r (hop-proc sl))
               (list (minus-one-before-zero (list-ref sl 0))
                     (minus-one-before-zero (list-ref sl 1))
                     (minus-one-before-zero (list-ref sl 2))))]))))
