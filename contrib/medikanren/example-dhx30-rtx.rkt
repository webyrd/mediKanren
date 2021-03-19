#lang racket
(provide
  (all-from-out "../../medikanren/mk.rkt")
  (all-from-out "../../medikanren/mk-db.rkt")

  membero
  edgeo
  ~name-concepto

  rtx)


(require
  "mk.rkt"
  "mk-db.rkt"
  (except-in racket/match ==))


(define rtx (time (make-db "data/rtx")))

(newline)
(displayln "X regulates DHX30:")
(time (pretty-print
        (run* (edge)
          (fresh (eid subject object pred eprops)
            (== `(,eid ,subject ,object ,pred . ,eprops) edge)
            (db:~name-concepto rtx "DHX30" object)
            (== `(3 . "regulates") pred)
            (db:edgeo rtx edge)))))


#|
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
(displayln "fuzzy search:")
(time (pretty-print (run* (c) (db:~name-concepto rtx "imatin" c))))

(newline)
(displayln "fuzzy search DHX30:")
(time (pretty-print (run* (c) (db:~name-concepto rtx "DHX30" c))))
|#

#|
(newline)
(displayln "X ANY-PRED DHX30:")
(time (pretty-print (run* (edge)
                      (fresh (scid scui sname scatid scat sprops
                                   ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                        (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                   (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                   (,pid . ,pred) ,eprops) edge)
                        (db:~name-concepto rtx "DHX30" `(,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops)))
                        (db:edgeo rtx edge)))))

(newline)
(displayln "DHX30 ANY-PRED X")
(time (pretty-print (run* (edge)
                      (fresh (scid scui sname scatid scat sprops
                                   ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                        (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                   (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                   (,pid . ,pred) ,eprops) edge)
                        (db:~name-concepto rtx "DHX30" `(,scid . (,scui ,sname (,scatid . ,scat) . ,sprops)))
                        (db:edgeo rtx edge)))))
|#
