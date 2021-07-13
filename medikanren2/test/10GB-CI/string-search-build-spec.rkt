#lang racket
;(require racket)
;(require racket/match)
(require chk)
(require "../../common.rkt")
(require "../../string-search-impl.rkt")
(require "../../string-search.rkt")
(require racket/pretty)
(require "../../db/yeast-micro-sri-reference-kg-0.3.0.rkt")
(require "../../base.rkt")
(require "../../string-search.rkt")


(chk
    (#:do
        (let* (
                (relp-index (hash-ref (relation-definition-info nodes) 'path))
                (absd-index (current-config-relation-path relp-index))
                (name-corpus (test:read-name-corpus fn-cprop-primary absd-index))
                (nbytes-total
                    (time (for/sum ((v name-corpus))
                        (string-length (car v)))))
                (nbytes-avg (exact->inexact (/ nbytes-total (vector-length name-corpus)))))
            (printf "found ~a bytes, ~a items, ~a bytes/item\n" nbytes-total (vector-length name-corpus) nbytes-avg)
        ))
    (#:t #t))


(define fn-perftest (format "~a-perftest" fn-concept-name-index))

(define (absf-perftest rel)
    (let* (
            (relp-index (hash-ref (relation-definition-info nodes) 'path))
            (absd-index (current-config-relation-path relp-index)))
        (path->string (simplify-path (build-path absd-index fn-perftest)))))

(chk
    (#:do
        (test:ensure-name-index-built (hash-ref (relation-definition-info nodes) 'path) fn-perftest))
    (#:do
        (time (length (name-string-matches nodes '("CACNA1")))))
    (#:do
        (delete-file (absf-perftest nodes)))
    (#:t #t))
