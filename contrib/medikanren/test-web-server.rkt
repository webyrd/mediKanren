#lang racket
(require net/http-client)

(define (read/string s) (with-input-from-string s (lambda () (read))))
(define (write/string datum) (with-output-to-string (lambda () (write datum))))
(define (pretty/string datum)
  (with-output-to-string (lambda () (pretty-print datum))))

(define (query sdatum)
  (define-values (bstatus headers in)
    (http-sendrecv "localhost" "/query"
                   #:port 8000
                   #:ssl? #f
                   #:version "1.1"
                   #:method "POST"
                   #:data (write/string sdatum)))
  (define status (bytes->string/utf-8 bstatus))
  (define response (port->string in))
  (close-input-port in)
  (cond ((string-suffix? status "200 OK") (read/string response))
        (else (printf "~a\n" response)
              (error "Query failed:" status (map bytes->string/utf-8 headers)
                     response))))

(define (decreases? p)
  (member (cddr p) '("negatively_regulates"
                     "prevents"
                     "treats"
                     "disrupts"
                     "increases_degradation_of"
                     "decreases_activity_of"
                     "decreases_expression_of")))
(define (increases? p)
  (member (cddr p) '("positively_regulates"
                     "produces"
                     "causes"
                     "causes_condition"
                     "causally_related_to"
                     "contributes_to"
                     "gene_associated_with_condition"
                     "gene_mutations_contribute_to"
                     "decreases_degradation_of"
                     "increases_activity_of"
                     "increases_expression_of")))

(define SP (map (lambda (cp)
                  (define c (car cp))
                  (define pS (cadr cp))
                  (list c (filter decreases? pS) #f))
                (take (query '(concept #t #f 0 #f ("imatin"))) 1)))

(define OP (map (lambda (cp)
                  (define c (car cp))
                  (define pO (caddr cp))
                  (list c #f (filter increases? pO)))
                (take (query '(concept #f #t 0 #f ("asthma"))) 1)))

(displayln "Imatinib:")
(pretty-print SP)
(newline)
(displayln "Asthma:")
(pretty-print OP)

(define X (query `(X ,SP ,OP)))
(define x-bcr
  (car (filter (lambda (x) (equal? (cadddr (car x)) "BCR gene")) X)))

(newline)
(displayln 'X:)
(pretty-print (car x-bcr))
(newline)
(displayln 'S-edges:)
(pretty-print (cadr x-bcr))
(newline)
(displayln 'O-edges:)
(pretty-print (caddr x-bcr))
