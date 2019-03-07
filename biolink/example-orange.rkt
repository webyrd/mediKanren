#lang racket

(provide
  (all-from-out "mk-db.rkt")
  (all-defined-out)
  )

(require
  "mk-db.rkt"
  )

(define orange (time (make-db "data/orange")))

(displayln "categories:")
(pretty-print (run* (c) (db:categoryo orange c)))

(newline)
(displayln "predicates:")
(pretty-print (run* (p) (db:predicateo orange p)))

(newline)
(displayln "some concepts:")
(pretty-print (run 10 (c) (db:concepto orange c)))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (db:edgeo orange e)))
