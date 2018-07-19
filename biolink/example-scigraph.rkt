#lang racket
(require
  "mk-db.rkt"
  )

(define scigraph (time (make-db "data/scigraph")))

(displayln "categories:")
(pretty-print (run* (c) (db:categoryo scigraph c)))

(newline)
(displayln "predicates:")
(pretty-print (run* (p) (db:predicateo scigraph p)))

(newline)
(displayln "some concepts:")
(pretty-print (run 10 (c) (db:concepto scigraph c)))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (db:edgeo scigraph e)))

(newline)
(displayln "fuzzy search (still a little slow, will improve):")
(time (pretty-print (run* (i v) (db:~name-concepto scigraph "imatin" i v))))
