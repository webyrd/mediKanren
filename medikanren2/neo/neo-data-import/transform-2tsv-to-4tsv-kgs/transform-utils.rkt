#lang racket
(provide efficient-no-trim-tab-string-split
         build-buckets-with-top
         build-buckets-with-interval
         build-pred-score-amount-hash)

#|

Racket's `string-split` function has lots of functionality, but can be
*extremely* slow for very long strings (with millions of characters, for
example):

https://docs.racket-lang.org/reference/strings.html#%28def._%28%28lib._racket%2Fstring..rkt%29._string-split%29%29

For example, splitting a single extremely long string may take minutes
or even hours.

Instead of using `string-split`, the KG TSV processing code for the
neo server uses a more limited, but far more efficient, function for
splitting a string into substrings, based on the tab character as a
delimiter.

|#

(define efficient-no-trim-tab-string-split
  (lambda (str)
    (let ((len (string-length str)))
      (let loop ((i 0)
                 (indices '(-1)))
        (cond
          [(= i len)
           ;; create substrings from the numeric offsets, and return a
           ;; list of these substrings
           (let inner-loop ((indices (reverse (cons (string-length str) indices)))
                            (strings '()))
             (cond
               [(null? indices)
                (error 'efficient-no-trim-tab-string-split
                       (format "unexpected unmatched indices"))]
               [(null? (cdr indices))
                (reverse strings)]
               [else
                (inner-loop
                 (cdr indices)
                 (cons (substring str (add1 (car indices)) (cadr indices))
                       strings))]))]
          [(char=? (string-ref str i) #\tab)
           (loop
            (add1 i)
            (cons i indices))]
          [else
           (loop
            (add1 i)
            indices)])))))

(define (build-buckets-with-top top n)
  (if (< n top)
      n
      top))

; 0 base
(define (index-of lst ele)
  (let loop ((l lst)
             (i 0))
    (cond
      [(null? l) #f]
      [(equal? (car l) ele) i]
      [else (loop (cdr l) (add1 i))])))

;; (list 0 1 2 (3 . 4) 5) == 0-0, 1-1, 2-2, 3-4, 5&5+
(define (build-buckets-with-interval interval* n)
  (define (helper new-interval*)
    (cond
      [(null? (cdr new-interval*)) (- (length interval*) 1)]
      [(equal? (car new-interval*) n) (index-of interval* n)]
      [(pair? (car new-interval*)) (if (and (not (< n (caar new-interval*)))
                                            (not (> n (cdar new-interval*))))
                                       (index-of interval* (car new-interval*))
                                       (helper (cdr new-interval*)))]
      [else (helper (cdr new-interval*))]))
  (helper interval*))

(define (build-pred-score-amount-hash path)
  (define in
      (open-input-file path))
  (define return (make-hash))
  (let loop ((line (read-line in 'any)))
    (cond
      ((eof-object? line)
       (close-input-port in)
        return)
      (else
        (let* ((line (efficient-no-trim-tab-string-split line))
               (predicate (list-ref line 0))
               (score (string->number  (list-ref line 1)))
               (amount (string->number (list-ref line 2))))
          (hash-set! return predicate (hash-set (hash-ref return predicate (hash))
                                                score
                                                amount))
          (loop (read-line in 'any)))))))
                                 
             
    