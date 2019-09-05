#lang racket

(provide
  (all-from-out "mk.rkt")
  (all-from-out "db.rkt")
  (all-from-out "mk-db.rkt")
  (all-from-out "common.rkt")  
  (all-from-out racket/date)
  (all-defined-out))

(require
  "mk.rkt"
  "db.rkt"
  "mk-db.rkt"
  "common.rkt"
  "create-all-hashtables.rkt"
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
(define semmed (make-db "data/semmed"))
(displayln "loading rtx knowledge graph")
(define rtx (make-db "data/rtx"))
(displayln "loading robokop knowledge graph")
(define robokop (make-db "data/robokop"))
(displayln "loading orange knowledge graph")
(define orange (make-db "data/orange"))

(printf "\n\nfull semmed edge:\n")
(run 1 (edge) (db:edgeo semmed edge))

(printf "\n\nfull robokop edge:\n")
(run 1 (edge) (db:edgeo robokop edge))

(printf "\n\nfull rtx edge:\n")
(run 1 (edge) (db:edgeo rtx edge))

(printf "\n\nfull orange edge:\n")
(run 1 (edge) (db:edgeo robokop edge))
