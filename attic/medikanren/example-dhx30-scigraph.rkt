#lang racket
(require
  "mk-db.rkt"
  )

(define scigraph (time (make-db "data/scigraph")))

;; list membership
(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        [(== x y)]
        [(=/= x y) (membero x rest)]))))


#|
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
(displayln "fuzzy search:")
(time (pretty-print (run* (c) (db:~name-concepto scigraph "imatin" c))))
|#

#|
(newline)
(displayln "fuzzy search DHX30:")
(time (pretty-print (run* (c) (db:~name-concepto scigraph "DHX30" c))))
|#

#|
(newline)
(displayln "fuzzy search for DHX30 synonyms:")
(time (pretty-print (run* (c)
                      (fresh (name)
                        ;; Gene Card synonyms
                        ;; https://www.genecards.org/cgi-bin/carddisp.pl?gene=DHX30&keywords=DHX30
                        (membero name '("DHX30"
                                        "DExH-Box Helicase 30"
                                        "DEAD/H (Asp-Glu-Ala-Asp/His) Box Polypeptide 30"
                                        "DEAH-Box Helicase 30"
                                        "DEAH Box Protein 30"
                                        "DDX30"
                                        "DEAH (Asp-Glu-Ala-His) Box Polypeptide 30"
                                        "Putative ATP-Dependent RNA Helicase DHX30"
                                        "DEAH (Asp-Glu-Ala-His) Box Helicase 30"
                                        "Retina Co-Repressor"
                                        "EC 3.6.4.13"
                                        "KIAA0890"
                                        "NEDMIAL"
                                        "RETCOR"
                                        ))
                        (project (name)
                          (db:~name-concepto scigraph name c))))))
|#



(newline)
(displayln "X ANY-PRED ANY-DHX30-SYNONYM:")
(time (pretty-print (run* (q)
                      (fresh (edge name)
                        (fresh (scid scui sname scatid scat sprops
                                     ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                          (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                     (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                     (,pid . ,pred) ,eprops) edge)
                          (membero name '("DHX30"
                                            "DExH-Box Helicase 30"
                                            "DEAD/H (Asp-Glu-Ala-Asp/His) Box Polypeptide 30"
                                            "DEAH-Box Helicase 30"
                                            "DEAH Box Protein 30"
                                            "DDX30"
                                            "DEAH (Asp-Glu-Ala-His) Box Polypeptide 30"
                                            "Putative ATP-Dependent RNA Helicase DHX30"
                                            "DEAH (Asp-Glu-Ala-His) Box Helicase 30"
                                            "Retina Co-Repressor"
                                            "EC 3.6.4.13"
                                            "KIAA0890"
                                            "NEDMIAL"
                                            "RETCOR"
                                            ))
                          (project (name)
                            (db:~name-concepto scigraph name `(,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))))
                          (== `(,sname ,pred ,oname) q)
                          (db:edgeo scigraph edge))))))


#|
(newline)
(displayln "X ANY-PRED DHX30:")
(time (pretty-print (run* (edge)
                      (fresh (scid scui sname scatid scat sprops
                                   ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                        (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                   (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                   (,pid . ,pred) ,eprops) edge)
                        (db:~name-concepto scigraph "DHX30" `(,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops)))
                        (db:edgeo scigraph edge)))))

(newline)
(displayln "DHX30 ANY-PRED X")
(time (pretty-print (run* (edge)
                      (fresh (scid scui sname scatid scat sprops
                                   ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                        (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                   (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                   (,pid . ,pred) ,eprops) edge)
                        (db:~name-concepto scigraph "DHX30" `(,scid . (,scui ,sname (,scatid . ,scat) . ,sprops)))
                        (db:edgeo scigraph edge)))))
|#
