#lang racket/base
(require
  "query-low-level.rkt"
  racket/pretty)

(define close-match-preds '("biolink:close_match"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find ORPHANET curies that are close matches to ICD9 or ICD10 curies ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define prefix.ORPHANET "ORPHANET:")
(define prefix.ICD9     "ICD9:")
(define prefix.ICD10    "ICD10:")

(newline)
(define ORPHANET-ICD9-matches (query:Prefix->Prefix prefix.ORPHANET close-match-preds prefix.ICD9))
(pretty-write `(ORPHANET-ICD9-matches count: ,(length ORPHANET-ICD9-matches)))
(pretty-write ORPHANET-ICD9-matches)

(newline)
(define ORPHANET-ICD10-matches (query:Prefix->Prefix prefix.ORPHANET close-match-preds prefix.ICD10))
(pretty-write `(ORPHANET-ICD10-matches count: ,(length ORPHANET-ICD10-matches)))
(pretty-write ORPHANET-ICD10-matches)
