#lang racket
(provide
  (all-from-out "mk.rkt")
  (all-from-out "mk-db.rkt")

  membero
  rem-dups
  edgeo
  ~name-concepto
  DECREASES

  semmed
  monarch
  rtx
  scigraph
  )


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

;; remove duplicates from a list
(define rem-dups
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(member (car ls) (cdr ls)) (rem-dups (cdr ls))]
      [else (cons (car ls) (rem-dups (cdr ls)))])))


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


(define (DECREASES pred)
  (fresh (_)
    (conde
      [(== `(,_ . "treats") pred)]
      [(== `(,_ . "prevents") pred)]
      [(== `(,_ . "negatively_regulates") pred)])))

(displayln "preds for X ANY-PRED CACNA1A")
(time (rem-dups
        (run* (q)
            (fresh (edge eid tacrine-details subject object pred eprops db)
              (== `(,eid ,subject ,object ,pred . ,eprops) edge)
              (~name-concepto "CACNA1A" `(,db . ,object))            
              (edgeo `(,db . ,edge))
              (== pred q)))))
;; =>
'((0 . "interacts_with")
  (3 . "part_of")
  (5 . "negatively_regulates")
  (6 . "coexists_with")
  (8 . "related_to")
  (9 . "affects")
  (10 . "produces")
  (13 . "positively_regulates")
  (0 . "directly_interacts_with")
  (6 . "part_of")
  (3 . "interacts_with")
  (3 . "regulates")
  (10 . "coexists_with")
  (16 . "location_of")
  (17 . "produces")
  (18 . "positively_regulates")
  (19 . "negatively_regulates"))


(displayln "X ANY-PRED CACNA1A")
(time (pretty-print
       (run* (q)
         (fresh (edge name db)
           (fresh (subject scid scui sname sdetails
                           object ocid ocui oname odetails
                           eid pid pred eprops)
             (== `(,scid ,scui ,sname . ,sdetails) subject)
             (== `(,ocid ,ocui ,oname . ,odetails) object)
             (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
             (~name-concepto "CACNA1A" `(,db . ,object))
             (== `(,sname ,pred ,oname) q)
             (edgeo `(,db . ,edge)))))))

(newline)
(displayln "X DECREASES CACNA1A:")
(time (pretty-print
        (run* (edge)
          (fresh (eid tacrine-details subject object pred eprops db)
            (== `(,eid ,subject ,object ,pred . ,eprops) edge)
            (~name-concepto "CACNA1A" `(,db . ,object))
            (DECREASES pred)
            (edgeo `(,db . ,edge))))))
