#lang racket
(require
  "mk-db.rkt"
  )

(define semmed (make-db "data/semmed"))


(displayln "------- DHX30 examples, using semmed ---------")

(newline)
(displayln "fuzzy search for DHX30:")
(time (pretty-print (run* (c) (db:~name-concepto semmed "DHX30" c))))
;; =>
'((107213
   "UMLS:C1424502"
   "DHX30 gene"
   (4 . "gene")
   (("umls_type_label" . "['Gene or Genome']")
    ("xrefs" . "['OMIM:616423', 'HGNC:HGNC:16716', 'MTH:NOCODE']")
    ("id" . "UMLS:C1424502")
    ("umls_type" . "['T028']")
    ("labels" . "['gene']"))))

(newline)
(displayln "semmedb predicates:")
(pretty-print (run* (p) (db:predicateo semmed p)))
;; =>
'((0 . "interacts_with")
  (1 . "treats")
  (2 . "predisposes")
  (3 . "part_of")
  (4 . "subclass_of")
  (5 . "negatively_regulates")
  (6 . "coexists_with")
  (7 . "causes")
  (8 . "related_to")
  (9 . "affects")
  (10 . "produces")
  (11 . "gene_associated_with_condition")
  (12 . "manifestation_of")
  (13 . "positively_regulates")
  (14 . "prevents")
  (15 . "derives_into")
  (16 . "location_of")
  (17 . "precedes"))

(newline)
(displayln "X DECREASES DHX30:")
(time (pretty-print (let ((DECREASES (lambda (pred-info)
                                       (conde
                                         [(== `(1 . "treats") pred-info)]
                                         [(== `(14 . "prevents") pred-info)]))))
                      (run* (edge)
                        (fresh (scid scui sname scatid scat sprops
                                     ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                          (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                     (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                     (,pid . ,pred) ,eprops) edge)
                          (db:~name-concepto semmed "DHX30" `(,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops)))
                          (DECREASES `(,pid . ,pred))
                          (db:edgeo semmed edge))))))

(newline)
(displayln "X ANY-PRED DHX30:")
(time (pretty-print (run* (edge)
                      (fresh (scid scui sname scatid scat sprops
                                   ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                        (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                   (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                   (,pid . ,pred) ,eprops) edge)
                        (db:~name-concepto semmed "DHX30" `(,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops)))
                        (db:edgeo semmed edge)))))

(newline)
(displayln "DHX30 ANY-PRED X")
(time (pretty-print (run* (edge)
                      (fresh (scid scui sname scatid scat sprops
                                   ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                        (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                   (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                   (,pid . ,pred) ,eprops) edge)
                        (db:~name-concepto semmed "DHX30" `(,scid . (,scui ,sname (,scatid . ,scat) . ,sprops)))
                        (db:edgeo semmed edge)))))
