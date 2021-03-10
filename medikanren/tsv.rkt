#lang racket/base
(provide tsv-records)
(require racket/string)

;; Informal grammar for tab-separated values (no escapes supported)
;FIELD-SEPARATOR  ::= \t
;RECORD-SEPARATOR ::= \r\n | \n | \r
;record-stream    ::= EOF | record EOF | record RECORD-SEPARATOR record-stream
;record           ::= field | field FIELD-SEPARATOR record
;field            ::= CONTENT*
;CONTENT includes anything other than \t, \n, \r

(define (tsv-records in)
  (define (next-record)
    (define l (read-line in 'any))
    (and (not (eof-object? l)) (string-split l "\t" #:trim? #f)))
  (define current #f)
  (lambda (request)
    (case request
      ((current) current)
      ((next)    (set! current (next-record)) current)
      (else      (error "invalid tsv-records request:" request)))))
