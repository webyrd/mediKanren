#lang racket
(require "../common.rkt" "../mk-db.rkt" "propagator.rkt")
(load-databases #t)

(define positively-regulates '("causes"))
(define negatively-regulates '("negatively_regulates"))

(define imatinib "UMLS:C0935989")
(define asthma   "UMLS:C0004096")

(define S (concept/curie imatinib))
(define X (concept/any))
(define O (concept/curie asthma))

(define S->X (edge/predicate negatively-regulates S X))
(define X->O (edge/predicate positively-regulates X O))


(displayln 'S)
(S 'ref)

(displayln 'O)
(O 'ref)

(displayln 'X)
(X 'ref)

(displayln 'S->X)
(S->X 'ref)

(displayln 'X->O)
(X->O 'ref)

(time (run!))

;; examine results
;(X 'ref)
;(S->X 'ref)
;(X->O 'ref)
