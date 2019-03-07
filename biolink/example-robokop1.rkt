#lang racket

(provide
  (all-from-out "mk-db.rkt")
  (all-defined-out)
  )

(require
  "mk-db.rkt"
  )

(define robokop1 (time (make-db "data/robokop1")))

(displayln "categories:")
(pretty-print (run* (c) (db:categoryo robokop1 c)))

(newline)
(displayln "predicates:")
(pretty-print (run* (p) (db:predicateo robokop1 p)))

(newline)
(displayln "some concepts:")
(pretty-print (run 10 (c) (db:concepto robokop1 c)))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (db:edgeo robokop1 e)))
