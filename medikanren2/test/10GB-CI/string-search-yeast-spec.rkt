#lang racket
;(require racket)
(require chk)
(require "../../db/yeast-sri-reference-kg-0.3.0.rkt")
(require "../../base.rkt")
(require "../../string-search.rkt")
(require "../../profiling.rkt")

;;; Can we perform lookups with the string search API?

;; Does CACNA1 have at least 500 IDs?  (Most recent has 1185)
(chk
 (#:t
  (>=
   (length (name-string-matches nodes '("CACNA1")))
   500)))

(define cacna1-names
  `(("ClinVarVariant:11614" "NM_001256789.3(CACNA1F):c.1106G>A (p.Gly369Asp)")
    ("ClinVarVariant:11615" "NM_001256789.3(CACNA1F):c.2872C>T (p.Arg958Ter)")
    ("ClinVarVariant:11616" "CACNA1F, 1-BP INS, 991C")
    ("ClinVarVariant:11617" "NM_001256789.3(CACNA1F):c.2650C>T (p.Arg884Ter)")
    ("ClinVarVariant:11618" "CACNA1F, 1-BP DEL, 4548C")))

;; name-string-matches: Do we find familiar IDs named CACNA1?
(chk
 (#:=
  (take
   (sort
    (name-string-matches nodes '("CACNA1"))
    string<?)
   5)
  (map car cacna1-names)
  ))

;; find-concept-named: Do we find familiar IDs named CACNA1?
(chk
 (#:=
  (take
   (sort
    (run* (id object)
          (fresh (subj)
                 ((find-concept-named nodes '("CACNA1")) id subj object)
                 (== subj "name")))
    string<?
    #:key car)
   5)
  cacna1-names))


;; If we look up the names of our familar IDs, are they still the same?
(chk
 (#:=
  (map
   cadr
   (sort
    (run* (id object)
          (fresh (subj)
                 (nodes id subj object)
                 (== subj "name")
                 (membero id (map car cacna1-names))))
    string<?
    #:key car))
  (map cadr cacna1-names)))

;; Multiple strings passed to name-string-matches should be interpreted as set intersection.
(chk
  (#:do (define a "CACNA"))
  (#:do (define b "2D1"))
  (#:do (define res-a (name-string-matches nodes `(,a) (make-stsopt))))
  (#:do (define res-b (name-string-matches nodes `(,b) (make-stsopt))))
  (#:do (define res-ab (name-string-matches nodes `(,a ,b) (make-stsopt))))
  ;(#:do (printf "found ~s ~s ~s\n" (length res-a) (length res-b) (length res-ab)))
  ; .../yeast-sri-reference/0.3.0
  ; found 1412 1374 49
  (#:t (> (length res-a) 0))
  (#:t (> (length res-b) 0))
  (#:t (> (length res-ab) 0))
  (#:t (> (length res-a) (length res-ab)))
  (#:t (> (length res-b) (length res-ab)))
  )

(chk
  (#:do (define a "cacna"))
  (#:do (define b "2d1"))
  (#:do (define stsopt1 (make-stsopt #:case-sensitive? #t)))
  (#:do (define res-a (name-string-matches nodes `(,a) stsopt1)))
  (#:do (define res-b (name-string-matches nodes `(,b) stsopt1)))
  (#:do (define res-ab (name-string-matches nodes `(,a ,b) stsopt1)))
  (#:do (printf "found ~s ~s ~s\n" (length res-a) (length res-b) (length res-ab)))
  ; .../yeast-sri-reference/0.3.0
  ; found 650 736 21
  (#:t #t))
