#lang racket
(require
  "mk-db.rkt"
  )

(displayln "loading semmed")
(define semmed (time (make-db "data/semmed")))
(displayln "loading monarch-lite")
(define monarch (time (make-db "data/monarch-lite")))
(displayln "loading rtx")
(define rtx (time (make-db "data/rtx")))
(displayln "loading scigraph")
(define scigraph (time (make-db "data/scigraph")))

(define (edgeo e)
  (conde
    ((fresh (ee) (== `(semmed . ,ee) e) (db:edgeo semmed ee)))
    ((fresh (ee) (== `(monarch . ,ee) e) (db:edgeo monarch ee)))
    ((fresh (ee) (== `(rtx . ,ee) e) (db:edgeo rtx ee)))
    ((fresh (ee) (== `(scigraph . ,ee) e) (db:edgeo scigraph ee)))))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (edgeo e)))
