#lang racket/base
(provide tsv->stream dsv->stream csv->stream
         path->format format->header/port->stream)
(require racket/function racket/list racket/string)

;; Informal grammar for tab-separated values (no escapes supported)
;FIELD-SEPARATOR  ::= \t
;RECORD-SEPARATOR ::= \r\n | \n | \r
;record-stream    ::= EOF | record EOF | record RECORD-SEPARATOR record-stream
;record           ::= field | field FIELD-SEPARATOR record
;field            ::= CONTENT*
;CONTENT includes anything other than \t, \n, \r

(define (tsv->stream header? in)
  (valid-header?! in header? "\t")
  (let loop () (thunk (define l (read-line in 'any))
                      (if (eof-object? l) '()
                        (cons (string-split l "\t" #:trim? #f) (loop))))))

;; Informal grammar for delimiter-separated values (escapes via double quote)
;RECORD-SEPARATOR ::= \r\n | \n | \r
;record-stream    ::= EOF | record EOF | record RECORD-SEPARATOR record-stream
;record           ::= field | field FIELD-SEPARATOR record
;field            ::= \" inner-content* \" | CONTENT*
;inner-content    ::= CONTENT | \"\" | FIELD-SEPARATOR | WHITESPACE
;CONTENT includes anything other than double-quote, field separator, whitespace

(define (csv->stream header? in) (dsv->stream #\, header? in))

(define (dsv->stream field-separator header? in)
  (define (field)
    (define ch (peek-char in))
    (cond ((eqv? ch field-separator) (read-char in) "")
          ((or (eqv? ch #\newline) (eqv? ch #\return) (eof-object? ch)) "")
          ((eqv? ch #\")
           (read-char in)
           (let loop ((i 0))
             (define ch (peek-char in i))
             (cond ((eqv? ch #\")
                    (if (eqv? (peek-char in (+ i 1)) #\") (loop (+ i 2))
                      (let ((qs (bytes->string/utf-8 (read-bytes i in))))
                        (read-char in)
                        (when (eqv? (peek-char in) field-separator)
                          (read-char in))
                        (string-replace qs "\"\"" "\""))))
                   (else (loop (+ i 1))))))
          (else (let loop ((i 1))
                  (define ch (peek-char in i))
                  (cond ((eqv? ch field-separator)
                         (define s (bytes->string/utf-8 (read-bytes i in)))
                         (read-char in) s)
                        ((or (eqv? ch #\newline) (eqv? ch #\return) (eof-object? ch))
                         (bytes->string/utf-8 (read-bytes i in)))
                        (else (loop (+ i 1))))))))
  (define (record)
    (cons (field)
          (let ((ch (peek-char in)))
            (cond ((eqv? ch #\return) (read-char in)
                                      (when (eqv? (peek-char in) #\newline)
                                        (read-char in))
                                      '())
                  ((eqv? ch #\newline) (read-char in) '())
                  ((eof-object? ch)                   '())
                  (else                               (record))))))
  (valid-header?! in header? (make-string 1 field-separator))
  (let loop () (thunk (if (eof-object? (peek-char in)) '()
                        (cons (record) (loop))))))

(define (valid-header?! in header delimiter)
  (define found    (and header (read-line in 'any)))
  (define expected (and header (cond ((eq? header #t)  found)
                                     ((string? header) header)
                                     (else (string-join header delimiter)))))
  (unless (equal? found expected)
    (error "invalid header:" 'found: found 'expected: expected)))

(define (path->format path)
  (define fname (if (path? path) (path->string path) path))
  (case (last (string-split fname "." #:trim? #f))
    (("tsv") 'tsv)
    (("csv") 'csv)
    (else    #f)))

(define (format->header/port->stream format)
  (case format
    ((tsv) tsv->stream)
    ((csv) csv->stream)
    (else  (error "invalid format:" format))))
