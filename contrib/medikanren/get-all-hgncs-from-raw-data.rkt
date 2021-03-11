#lang racket
(provide (all-defined-out)
         (all-from-out "pieces-parts/synonymize.rkt"))
(require "pieces-parts/synonymize.rkt")


;; rtx2 contains a bunch of weird entries of the form:
;;
;; "HGNC:24039/HGNC:<some number>"
;;
;; "HGNC:24039/HGNC:877"
;; "HGNC:24039/HGNC:2602"
;; "HGNC:24039/HGNC:470"
;;
;; These are presumably errors.  We filter these out.

(define get-hgncs-for-concept-file-path
  (lambda (concept-file-path)
    (let ((ip (open-input-file concept-file-path)))
      (let loop ([cid 0]
                 [data (read ip)]
                 [s (set)])
        (cond
          [(eof-object? data)
           (close-input-port ip)
           (printf "read ~s entries\n" cid)
           s]
          [else
           (when (= (modulo cid 10000) 0)
             (printf "cid: ~s\n" cid))
           (let ((s
                  (let ((curie (vector-ref data 0)))
                    (if (regexp-match #px"^HGNC:[0-9]*$" curie)
                        (set-add s curie)
                        s))))
             (loop (add1 cid) (read ip) s))])))))

(define (save-hgncs-to-file! s hgnc-file-path)
  (define op (open-output-file hgnc-file-path #:exists 'replace))
  (for-each (lambda (str) (write str op) (newline op)) (set->list s))
  (close-output-port op))


(printf "processing robokop-hgncs\n")
(define robokop-hgncs
  (get-hgncs-for-concept-file-path "data/robokop/concepts.scm"))

(printf "processing rtx2-hgncs\n")
(define rtx2-hgncs
  (get-hgncs-for-concept-file-path "data/rtx2/concepts.scm"))

(printf "sorting all hgncs\n")
(define all-hgncs
  (sort (set->list (set-union robokop-hgncs
                              rtx2-hgncs))
        (lambda (str1 str2)
          (let ((hgnc-curie->n (lambda (str)
                                 (string->number (cadr (regexp-match #px"^HGNC:([0-9]*)$" str))))))
            (let ((n1 (hgnc-curie->n str1))
                  (n2 (hgnc-curie->n str2)))
              (< n1 n2))))))

; (printf "saving all hgncs\n")
; (save-hgncs-to-file! all-hgncs "./all-hgnc-curies.scm")

(printf "synonymizing hgncs\n")
(define synonymized-hashes
  (map
    (lambda (curie)
      (printf "*** HGNC curie: ~s\n" curie)
      (HGNC-CURIE->synonymized-concepts curie))
    all-hgncs))

;; > (begin (HGNC-CURIE->synonymized-concepts '("HGNC:8")) 2)
;; car: contract violation
;;   expected: pair?
;;   given: '()
;; [,bt for context]

(define ht (make-hash))

(for-each
  (lambda (h)
    (let ((all-genes/proteins (set->list (hash-ref h 'all-genes/proteins))))
      (for-each
        (lambda (curie)
          (hash-set! ht curie h))
        all-genes/proteins)))
  synonymized-hashes)

;; TODO
;; save ht to disk!
