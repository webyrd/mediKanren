#lang racket
(provide
  (all-from-out "mk.rkt")
  (all-from-out "mk-db.rkt")

  membero
  edgeo
  ~name-concepto)


(require
  "mk.rkt"
  "mk-db.rkt"
  (except-in racket/match ==))

(displayln "loading semmed")
(define semmed (time (make-db "data/semmed")))
(displayln "loading monarch-lite")
(define monarch (time (make-db "data/monarch-lite")))
(displayln "loading rtx")
(define rtx (time (make-db "data/rtx")))
(displayln "loading scigraph")
(define scigraph (time (make-db "data/scigraph")))

;; list membership
(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        [(== x y)]
        [(=/= x y) (membero x rest)]))))

(define (edgeo e)
  (conde
    ((fresh (ee) (== `(semmed . ,ee) e) (db:edgeo semmed ee)))
    ((fresh (ee) (== `(monarch . ,ee) e) (db:edgeo monarch ee)))
    ((fresh (ee) (== `(rtx . ,ee) e) (db:edgeo rtx ee)))
    ((fresh (ee) (== `(scigraph . ,ee) e) (db:edgeo scigraph ee)))))

(define (~name-concepto n c)
  (conde
    ((fresh (cc) (== `(semmed . ,cc) c) (db:~name-concepto semmed n cc)))
    ((fresh (cc) (== `(monarch . ,cc) c) (db:~name-concepto monarch n cc)))
    ((fresh (cc) (== `(rtx . ,cc) c) (db:~name-concepto rtx n cc)))
    ((fresh (cc) (== `(scigraph . ,cc) c) (db:~name-concepto scigraph n cc)))))

(time (pretty-print
       (run* (concept)
         (~name-concepto "BRCA1" concept))))

(displayln "X ANY-PRED ANY-BRCA1-SYNONYM:")
(time (pretty-print
       (run* (q)
         (fresh (edge name)
           (fresh (subject sdb scid scui sname sdetails
                           object odb ocid ocui oname odetails
                           eid pid pred eprops)
             (== `(,scid ,scui ,sname . ,sdetails) subject)
             (== `(,ocid ,ocui ,oname . ,odetails) object)
             (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
             (membero name '("BRCA1"
                             "RING Finger Protein 53"
                             "RNF53"))
             (~name-concepto name `(,obd . ,object))
             (== `(,sname ,pred ,oname) q)
             (edgeo edge))))))

