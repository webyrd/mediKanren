#lang racket
(require
  "mk-db.rkt"
  )

(define semmed (time (make-db "data/semmed")))

(displayln "categories:")
(pretty-print (run* (c) (db:categoryo semmed c)))

(newline)
(displayln "predicates:")
(pretty-print (run* (p) (db:predicateo semmed p)))

(newline)
(displayln "some concepts:")
(pretty-print (run 10 (c) (db:concepto semmed c)))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (db:edgeo semmed e)))

(newline)
(displayln "fuzzy search (still a little slow, will improve):")
(time (pretty-print (run* (i v) (db:~name-concepto semmed "imatin" i v))))
