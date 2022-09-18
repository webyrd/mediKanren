#lang racket
(provide efficient-no-trim-tab-string-split)

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
