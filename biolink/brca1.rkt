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

(define x-anypred-anybrca1-synonym
  (lambda (db-name)
    (printf "loading db: ~s\n" db-name)
    (define db (time (make-db db-name)))
    (newline)
    (displayln "X ANY-PRED ANY-BRCA1-SYNONYM:")
    (time (pretty-print (run* (q)
                          (fresh (edge name)
                            (fresh (scid scui sname scatid scat sprops
                                         ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                              (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                         (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                         (,pid . ,pred) ,eprops) edge)
                              (membero name '("BRCA1"
                                              "RING Finger Protein 53"
                                              "RNF53"))
                              (project (name)
                                (db:~name-concepto db name `(,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))))
                              (== `(,sname ,pred ,oname) q)
                              (db:edgeo db edge))))))))


;(define db-name "data/monarch-lite")
(define db-name "data/rtx") ;; regulates
;(define db-name "data/scigraph")
;(define db-name "data/semmed")

(x-anypred-anybrca1-synonym db-name)
