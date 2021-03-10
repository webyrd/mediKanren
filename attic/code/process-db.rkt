#lang racket

;; Racket code to process the osteoporosis database, turning it into classy (but unnormalized) s-expressions using Racket.

;; https://docs.racket-lang.org/csv-reading/index.html

;; http://www.neilvandyke.org/racket/csv-reading/

;; https://stackoverflow.com/questions/10883426/how-to-download-and-parse-a-csv-file-in-racket

(require (planet neil/csv:2:0) net/url)

;; change me to point to your ncats directory!
(define ncats_proposal-location "/Users/webyrd/github/")

(define iver-url 
  (string->url
   (string-append "file://" ncats_proposal-location "ncats_proposal/ncats-proof-of-concept/raw-data/semmedVER30_A_clean.C0029456_osteoporosis.csv")))

(define data (csv->list (get-pure-port iver-url)))

(define symbolic-data
  (map (lambda (d)
         (list
          (string->number (list-ref d 0))
          (string->number (list-ref d 1))
          (string->number (list-ref d 2))
          (string->symbol (list-ref d 3))
          (string->symbol (list-ref d 4))
          (list-ref d 5)
          (string->symbol (list-ref d 6))
          (string->number (list-ref d 7))
          (string->symbol (list-ref d 8))
          (list-ref d 9)
          (string->symbol (list-ref d 10))
          (string->number (list-ref d 11))))
       data))

(call-with-output-file (string-append ncats_proposal-location "ncats_proposal/ncats-proof-of-concept/code/osteoporosis-data.scm")
  (lambda (output-port)
    (fprintf output-port "(define data '(\n")
    (let loop ([ls symbolic-data])
      (cond
        [(null? ls) (fprintf output-port "))\n" )]
        [else
         (fprintf output-port "~s\n" (car ls))
         (loop (cdr ls))]))))
