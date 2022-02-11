#lang racket

(provide
  (all-from-out "../../medikanren/mk.rkt")
  (all-from-out "../../medikanren/db.rkt")
  (all-from-out "../../medikanren/mk-db.rkt")
  (all-from-out "../../medikanren/common.rkt")
  (all-from-out racket/date)
  (all-defined-out))

(require
  "../../medikanren/mk.rkt"
  "../../medikanren/db.rkt"
  "../../medikanren/mk-db.rkt"
  "../../medikanren/common.rkt"
  "../../attic/medikanren/create-all-hashtables.rkt"
  racket/date
  (except-in racket/match ==))

#|
concept format (subject or object), without dbname at front:

`(,cid ,cui ,name (,catid . ,cat) . ,props)

concept format (subject or object), with dbname at front (as used in fuzzy-concepto):

`(,dbname ,cid ,cui ,name (,catid . ,cat) . ,props)


edge format, without dbname at front:

`(,eid (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
       (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
       (,pid . ,pred) . ,eprops)

edge format, with dbname at front (as used in edgeo):

`(,dbname ,eid (,scid ,scui ,sname (,scatid . ,scat) . ,sprops)
               (,ocid ,ocui ,oname (,ocatid . ,ocat) . ,oprops)
               (,pid . ,pred) . ,eprops)
|#


(displayln "loading semmed knowledge graph")
(define semmed (make-db "../../medikanren/data/semmed"))

(printf "\n\nfull semmed edge:\n")
(run 1 (edge) (db:edgeo semmed edge))

(printf "\n\nTyped query 1:\n")
(run 1 (q)
  (fresh (edge1
          eid1 scid1 scui1 sname1 scatid1 scat1 sprops1
          ocid1 ocui1 oname1 ocatid1 ocat1 oprops1
          pid1 pred1 eprops1)
    (== `(,eid1 (,scid1 ,scui1 ,sname1 (,scatid1 . ,scat1) . ,sprops1)
                (,ocid1 ,ocui1 ,oname1 (,ocatid1 . ,ocat1) . ,oprops1)
                (,pid1 . ,pred1) . ,eprops1)
        edge1)
    (== `(,sname1 ,pred1 ,oname1) q)
    (db:edgeo semmed edge1)))

(printf "\n\nTyped query 2:\n")
(run 1 (q)
  (fresh (edge1
          eid1 scid1 scui1 sname1 scatid1 scat1 sprops1
          ocid1 ocui1 oname1 ocatid1 ocat1 oprops1
          pid1 pred1 eprops1)
    (== `(,eid1 (,scid1 ,scui1 ,sname1 (,scatid1 . ,scat1) . ,sprops1)
                (,ocid1 ,ocui1 ,oname1 (,ocatid1 . ,ocat1) . ,oprops1)
                (,pid1 . ,pred1) . ,eprops1)
        edge1)
    (== '(5 . "chemical_substance") `(,scatid1 . ,scat1))
    (== `(,sname1 ,pred1 ,oname1) q)
    (db:edgeo semmed edge1)))

(printf "\n\nTyped query 3:\n")
(run 1 (q)
  (fresh (edge1
          eid1 scid1 scui1 sname1 scatid1 scat1 sprops1
          ocid1 ocui1 oname1 ocatid1 ocat1 oprops1
          pid1 pred1 eprops1)
    (== `(,eid1 (,scid1 ,scui1 ,sname1 (,scatid1 . ,scat1) . ,sprops1)
                (,ocid1 ,ocui1 ,oname1 (,ocatid1 . ,ocat1) . ,oprops1)
                (,pid1 . ,pred1) . ,eprops1)
        edge1)
    (== '(5 . "chemical_substance") `(,scatid1 . ,scat1))
    (== '(1 . "treats") `(,pid1 . ,pred1))
    (== `(,sname1 ,pred1 ,oname1) q)
    (db:edgeo semmed edge1)))
