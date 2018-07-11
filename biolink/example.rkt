#lang racket
(require
  "mk-db.rkt"
  )

(define semmed (make-db "data/semmed"))

(displayln "categories:")
(pretty-print (run* (i v) (db:categoryo semmed i v)))

(newline)
(displayln "predicates:")
(pretty-print (run* (i v) (db:predicateo semmed i v)))

(newline)
(displayln "some concepts:")
(pretty-print (run 10 (i v) (db:concepto semmed i v)))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (db:edgeo semmed e)))

(newline)
(displayln "fuzzy search (still a little slow, will improve):")
(time (pretty-print (run* (i v) (db:~name-concepto semmed "imatin" i v))))
