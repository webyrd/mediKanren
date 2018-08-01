#lang racket
(require
  "mk-db.rkt"
  )

(displayln "loading semmed")
(define semmed (time (make-db "data/semmed")))
(displayln "loading monarch-lite")
(define monarch (time (make-db "data/monarch-lite")))

(define (edgeo e)
  (conde
    ((fresh (ee) (== `(semmed . ,ee) e) (db:edgeo semmed ee)))
    ((fresh (ee) (== `(monarch . ,ee) e) (db:edgeo monarch ee)))))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (edgeo e)))
