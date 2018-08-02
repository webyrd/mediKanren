#lang racket
(provide
  (all-from-out "mk.rkt")
  (all-from-out "mk-db.rkt")

  membero
  edgeo
  ~name-concepto

  semmed
  monarch
  rtx
  scigraph)


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

(time (pretty-print
       (run* (concept)
         (~name-concepto "sialic acid" concept))))


;;; Error 1
(time (rem-dups
        (run* (q)
          (fresh (edge name)
            (fresh (subject sdb scid scui sname sdetails
                            object ocid ocui oname odetails
                            eid pid pred eprops)
              (== `(,scid ,scui ,sname . ,sdetails) subject)
              (== `(,ocid ,ocui ,oname . ,odetails) object)
              (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
              (conde
                [(db:~name-concepto semmed "sialic acid" object)]
                [(db:~name-concepto monarch "sialic acid" object)]
                [(db:~name-concepto rtx "sialic acid" object)]
                [(db:~name-concepto scigraph "sialic acid" object)])      
              (== (list pid pred) q)
              (conde
                [(db:edgeo semmed edge)]
                [(db:edgeo monarch edge)]
                [(db:edgeo rtx edge)]
                [(db:edgeo scigraph edge)]
                ))))))
; integer-bytes->integer: contract violation
;   expected: bytes?
;   given: #<eof>
;   argument position: 1st
; [,bt for context]


;;; Error 2
(displayln "X ANY-PRED sialic acid:")
(time (pretty-print
       (run* (q)
         (fresh (edge name)
           (fresh (subject sdb scid scui sname sdetails
                           object odb ocid ocui oname odetails
                           eid pid pred eprops)
             (== `(,scid ,scui ,sname . ,sdetails) subject)
             (== `(,ocid ,ocui ,oname . ,odetails) object)
             (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
             (~name-concepto "sialic acid" `(,odb . ,object))
             (== `(,sname ,pred ,oname) q)
             (edgeo edge))))))
; *: contract violation
;   expected: number?
;   given: '(#((unbound) (scope) 6270) #((unbound) (scope) 6271) #((unbound)
;     (scope) 6272) . #((unbound) (scope) 6273))
;   argument position: 1st
; [,bt for context]

#|
(run 1 (q)
  (fresh (edge name)
    (fresh (subject sdb scid scui sname sdetails
                    object ocid ocui oname odetails
                    eid pid pred eprops)
      (== `(,scid ,scui ,sname . ,sdetails) subject)
      (== `(,ocid ,ocui ,oname . ,odetails) object)
      (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
      (conde
        [(db:~name-concepto semmed "sialic acid" object)]
        [(db:~name-concepto monarch "sialic acid" object)]
        [(db:~name-concepto rtx "sialic acid" object)]
        [(db:~name-concepto scigraph "sialic acid" object)])      
      (== (list pid pred) q)
      (conde
        [(db:edgeo semmed edge)]
        [(db:edgeo monarch edge)]
        [(db:edgeo rtx edge)]
        [(db:edgeo scigraph edge)]))))

(run 1 (q)
  (fresh (edge name)
    (fresh (subject sdb scid scui sname sdetails
                    object ocid ocui oname odetails
                    eid pid pred eprops)
      (== `(,scid ,scui ,sname . ,sdetails) subject)
      (== `(,ocid ,ocui ,oname . ,odetails) object)
      (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
      (conde
        [(db:~name-concepto semmed "sialic acid" object)]
        [(db:~name-concepto monarch "sialic acid" object)]
        [(db:~name-concepto rtx "sialic acid" object)]
        [(db:~name-concepto scigraph "sialic acid" object)])      
      (== edge q)
      (conde
        [(db:edgeo semmed edge)]
        [(db:edgeo monarch edge)]
        [(db:edgeo rtx edge)]
        [(db:edgeo scigraph edge)]))))


(time (rem-dups (run* (q)
  (fresh (edge name)
    (fresh (subject sdb scid scui sname sdetails
                    object ocid ocui oname odetails
                    eid pid pred eprops)
      (== `(,scid ,scui ,sname . ,sdetails) subject)
      (== `(,ocid ,ocui ,oname . ,odetails) object)
      (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
      (conde
        [(db:~name-concepto semmed "sialic acid" object)]
        ;[(db:~name-concepto monarch "sialic acid" object)]
        ;[(db:~name-concepto rtx "sialic acid" object)]
        ;[(db:~name-concepto scigraph "sialic acid" object)]
        )      
      (== (list pid pred) q)
      (conde
        [(db:edgeo semmed edge)]
        [(db:edgeo monarch edge)]
        ;[(db:edgeo rtx edge)]
        [(db:edgeo scigraph edge)]
        ))))))
|#

#|
(displayln "X ANY-PRED sialic acid:")
(time (pretty-print
       (run* (q)
         (fresh (edge name)
           (fresh (subject sdb scid scui sname sdetails
                           object odb ocid ocui oname odetails
                           eid pid pred eprops)
             (== `(,scid ,scui ,sname . ,sdetails) subject)
             (== `(,ocid ,ocui ,oname . ,odetails) object)
             (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
             (~name-concepto "sialic acid" `(,odb . ,object))
             (== `(,sname ,pred ,oname) q)
             (edgeo edge))))))
|#

#|
(run 1 (q)
    (fresh (edge name)
      (fresh (subject sdb scid scui sname sdetails
                      object odb ocid ocui oname odetails
                      eid pid pred eprops)
        (== `(,scid ,scui ,sname . ,sdetails) subject)
        (== `(,ocid ,ocui ,oname . ,odetails) object)
        (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
        (== `(,sname ,pred ,oname) q)
        (db:edgeo scigraph edge))))
|#

#|
(run 1 (q)
    (fresh (edge name)
      (fresh (subject sdb scid scui sname sdetails
                      object odb ocid ocui oname odetails
                      eid pid pred eprops)
        (== `(,scid ,scui ,sname . ,sdetails) subject)
        (== `(,ocid ,ocui ,oname . ,odetails) object)
        (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
        (== `(,sname ,pred ,oname) q)
        (edgeo edge))))
|#
