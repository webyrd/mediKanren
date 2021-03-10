#lang racket/base
(provide
  csv-records
  set-field-separator!
  )


;; Informal CSV Grammar
;RECORD-SEPARATOR ::= \r\n | \n | \r
;record-stream ::= EOF | record EOF | record RECORD-SEPARATOR record-stream
;record ::= field | field FIELD-SEPARATOR record
;field ::= \" inner-content* \" | CONTENT*
;inner-content ::= CONTENT | \"\" | FIELD-SEPARATOR | WHITESPACE
;; CONTENT includes anything other than double-quote, field separator, whitespace

(define *field-separator* #\,)
(define (set-field-separator! ch) (set! *field-separator* ch))

(define (end-of-record?! in)
  (define ch (peek-char in))
  (or (and (char=? #\return ch)
           (or (and (char=? #\newline (peek-char in 1)) (read-string 2 in) #t)
               (and (read-char in) #t)))
      (and (char=? #\newline ch) (read-char in) #t)
      (eof-object? ch)))

(define (field-separator?! in)
  (define ch (peek-char in))
  (and (char=? *field-separator* ch) (read-char in) #t))

(define (end-of-field?! in)
  (define ch (peek-char in))
  (or (char=? *field-separator* ch)
      (char=? #\return ch) (char=? #\newline ch) (eof-object? ch)))

(define (field-quoted-begin?! in)
  (define ch (peek-char in))
  (and (char=? #\" ch) (read-char in) #t))

(define (field-quoted-end?! in)
  (define ch (peek-char in))
  (and (char=? #\" ch) (read-char in) (not (char=? #\" (peek-char in)))))

(define (csv-records in)
  (define (next-record)
    (and (not (eof-object? (peek-char in)))
         (let record ((continue? #t))
           (if continue?
             (cons (list->string
                     (let field ()
                       (cond ((end-of-field?! in) '())
                             ((field-quoted-begin?! in)
                              (let field-quoted ()
                                (if (field-quoted-end?! in) (field)
                                  (cons (read-char in) (field-quoted)))))
                             (else (cons (read-char in) (field))))))
                   (record (field-separator?! in)))
             (if (end-of-record?! in) '() (error "malformed csv"))))))
  (define current #f)
  (lambda (request)
    (case request
      ((current) current)
      ((next) (set! current (next-record)) current)
      (else (error "invalid csv-records request:" request)))))
