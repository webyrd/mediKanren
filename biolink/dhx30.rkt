#lang racket
(require
  "mk-db.rkt"
  )

;; list membership
(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        [(== x y)]
        [(=/= x y) (membero x rest)]))))

(define x-anypred-anydhx30-synonym
  (lambda (db-name)
    (printf "loading db: ~s\n" db-name)
    (define db (time (make-db db-name)))
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
                                (db:~name-concepto db name `(,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))))
                              (== `(,sname ,pred ,oname) q)
                              (db:edgeo db edge))))))))


;(define db-name "data/monarch-lite")
(define db-name "data/rtx") ;; regulates
;(define db-name "data/scigraph")
;(define db-name "data/semmed")

(x-anypred-anydhx30-synonym db-name)
