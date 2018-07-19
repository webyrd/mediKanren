#lang racket
(require
  "mk-db.rkt"
  )

(define monarch-lite (time (make-db "data/monarch-lite")))

(displayln "categories:")
(pretty-print (run* (c) (db:categoryo monarch-lite c)))

(newline)
(displayln "predicates:")
(pretty-print (run* (p) (db:predicateo monarch-lite p)))

(newline)
(displayln "some concepts:")
(pretty-print (run 10 (c) (db:concepto monarch-lite c)))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (db:edgeo monarch-lite e)))

(newline)
(displayln "fuzzy search:")
(time (pretty-print (run* (c) (db:~name-concepto monarch-lite "imatin" c))))
