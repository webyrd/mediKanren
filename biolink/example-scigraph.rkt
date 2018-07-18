#lang racket
(require
  "mk-db.rkt"
  )

(define scigraph (make-db "data/scigraph"))

(displayln "categories:")
(pretty-print (run* (i v) (db:categoryo scigraph i v)))

(newline)
(displayln "predicates:")
(pretty-print (run* (i v) (db:predicateo scigraph i v)))

(newline)
(displayln "some concepts:")
(pretty-print (run 10 (i v) (db:concepto scigraph i v)))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (db:edgeo scigraph e)))

(newline)
(displayln "fuzzy search (still a little slow, will improve):")
(time (pretty-print (run* (i v) (db:~name-concepto scigraph "imatin" i v))))
