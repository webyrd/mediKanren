#lang racket
(provide efficient-no-trim-tab-string-split
         build-pred-score-amount-hash
         find-index
         write-list-to-tsv
         unwrap)

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
                                

(define (find-index lst ele)
  (let loop ((lst lst) (idx 0))
    (cond
      [(null? lst) #f]
      [(equal? (car lst) ele) idx]
      [else (loop (cdr lst) (add1 idx))])))

(define write-list-to-tsv
  (lambda (header-ls lol path)
    (with-output-to-file path
      ;; thunk -- procedure that takes no arguments
      (lambda ()
        (for-each
          (lambda (l)
            (let loop ([l l])
              (cond
                ((null? l)
                 (error 'output-to-tsv "where's the data!?"))
                ((null? (cdr l)) ;; l contains exactly 1 element
                 (display (car l))
                 (display #\newline))
                (else
                 (display (car l))
                 (display #\tab)
                 (loop (cdr l))))))
          (cons header-ls lol)))
      #:mode 'text
      #:exists 'replace)))

(define unwrap
  (lambda (lol)
    (let loop ((lol lol) (accu '()))
      (cond
        ((null? lol) accu)
        ((not (pair? (car lol))) (cons lol accu))
        (else (append (loop (car lol) accu) (loop (cdr lol) accu)))))))
             
    