#lang racket
(require
  "mk-db.rkt"
  )

(define monarch-lite (time (make-db "data/monarch-lite")))

(displayln "categories:")
(pretty-print (run* (i v) (db:categoryo monarch-lite i v)))

(newline)
(displayln "predicates:")
(pretty-print (run* (i v) (db:predicateo monarch-lite i v)))

(newline)
(displayln "some concepts:")
(pretty-print (run 10 (i v) (db:concepto monarch-lite i v)))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (db:edgeo monarch-lite e)))

(newline)
(displayln "fuzzy search (still a little slow, will improve):")
(time (pretty-print (run* (i v) (db:~name-concepto monarch-lite "imatin" i v))))
