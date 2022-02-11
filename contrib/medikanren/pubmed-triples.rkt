#lang racket

;; sample usage
;; % racket
;; racket
;; Welcome to Racket v8.3 [cs].
;; > ,en pubmed-triples.rkt
;; ,en pubmed-triples.rkt
;; loading semmed
;; * cui-corpus:
;; cpu time: 802 real time: 810 gc time: 24
;; * name-corpus:
;; cpu time: 823 real time: 832 gc time: 28
;; * name-index:
;; cpu time: 9 real time: 9 gc time: 0
;; cpu time: 1862 real time: 1884 gc time: 70
;; "pubmed-triples.rkt"> (find-triples-by-pubmed-id "1000085")
;; (find-triples-by-pubmed-id "1000085")
;; finding edges for PubMed entry "1000085"
;; '(("High-Molecular-Weight Kininogen protein"
;;    "coexists_with"
;;    "plasma, human, normal")
;;   ("Plasma" "location_of" "Kinins")
;;   ("Plasma" "location_of" "High-Molecular-Weight Kininogen protein")
;;   ("Plasma" "location_of" "High-Molecular-Weight Kininogen protein")
;;   ("Normal plasma" "location_of" "High-Molecular-Weight Kininogen protein"))

(provide
 (all-from-out "../../medikanren/mk.rkt")
 (all-from-out "../../medikanren/db.rkt")
 (all-from-out "../../medikanren/mk-db.rkt")

 (all-defined-out))

(require
  "../../medikanren/mk.rkt"
  "../../medikanren/db.rkt"
  "../../medikanren/mk-db.rkt"
  (except-in racket/match ==))

(displayln "loading semmed")
(define semmed (time (make-db "../../medikanren/data/semmed")))

(define (edgeo e)
  (conde
    ((fresh (ee) (== `(semmed . ,ee) e) (db:edgeo semmed ee)))
    ))

(define ex-pid "1000085")

(define find-triples-by-pubmed-id
  (lambda (pubmed-id)
    (let ((pubmed-id-str (if (string? pubmed-id)
                             pubmed-id
                             (~a pubmed-id))))
      (printf "finding edges for PubMed entry ~s\n" pubmed-id-str)
      (run* (q)
        (fresh (dbname eid e
                       scid scui sname scatid scat sprops
                       ocid ocui oname ocatid ocat oprops
                       pid pred eprops)
          (db:pmid-eido semmed pubmed-id-str eid)
          (== `(,dbname ,eid (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
                        (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
                        (,pid . ,pred) . ,eprops)
              e)
          (== (list sname pred oname) q)
          (edgeo e))))))
