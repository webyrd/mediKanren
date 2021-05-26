#lang racket/base
(require
 "../base.rkt"
 (prefix-in semmed: "../db/semmed.rkt")
 (prefix-in rtx: "../db/rtx2-20210204.rkt")
 "../db/kgx-synonym.rkt"
 ;(prefix-in kgx: )
 ;"../db/sri-reference-kg-0.3.0.rkt"
 racket/pretty)

#|
Question 1
- When multiple KGs are loaded with the same names for their materialized relations cprop/eprop/edge, is there any error when queries 

Question 2 
- How does prefix in work with sri-reference-kg-0.3.0.rkt

|#

;; relation cprop exists in rtx2, semmed
#|
(run 5
     (c k v)
       (cprop c k v))
|#

;; relation synonym should exist in kgx-synonym 
(run 5
     (a b)
       (synonym '(a) "CHEBI:28304"))

