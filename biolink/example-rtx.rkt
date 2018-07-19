#lang racket
(require
  "mk-db.rkt"
  )

(define rtx (time (make-db "data/rtx")))

(displayln "categories:")
(pretty-print (run* (c) (db:categoryo rtx c)))

(newline)
(displayln "predicates:")
(pretty-print (run* (p) (db:predicateo rtx p)))

(newline)
(displayln "some concepts:")
(pretty-print (run 10 (c) (db:concepto rtx c)))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (db:edgeo rtx e)))

(newline)
(displayln "fuzzy search (still a little slow, will improve):")
(time (pretty-print (run* (i v) (db:~name-concepto rtx "imatin" i v))))
