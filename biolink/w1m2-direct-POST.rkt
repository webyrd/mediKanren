#lang racket
(require net/http-client
         json)

(define data (jsexpr->bytes
               '(
                 ("UMLS:C0282687" "Ebola hemorrhagic fever")
                 ;("OMIM:600807"   "Asthma")
                 ;("OMIM:215400"   "Chordoma")
                 ;("OMIM:125853"   "Type 2 Diabetes Mellitus")
                 )))

(displayln `(sending: ,data))

(define-values (status headers in)
  (http-sendrecv "localhost"
                 "/direct-query"
                 #:port 8000
                 #:ssl? #f
                 #:version "1.1"
                 #:method "POST"
                 #:data data))

(displayln status)
(displayln headers)
(displayln (port->string in))
(close-input-port in)
