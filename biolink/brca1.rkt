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
    (time (pretty-print
            (run* (q)
              (fresh (edge name)
                (fresh (subject scid scui sname sdetails
                        object ocid ocui oname odetails
                        eid pid pred eprops)
                  (== `(,scid ,scui ,sname . ,sdetails) subject)
                  (== `(,ocid ,ocui ,oname . ,odetails) object)
                  (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
                  (membero name '("BRCA1"
                                  "RING Finger Protein 53"
                                  "RNF53"))
                  (db:~name-concepto db name object)
                  (== `(,sname ,pred ,oname) q)
                  (db:edgeo db edge))))))))


;(define db-name "data/monarch-lite")
(define db-name "data/rtx") ;; regulates
;(define db-name "data/scigraph")
;(define db-name "data/semmed")

(x-anypred-anybrca1-synonym db-name)
