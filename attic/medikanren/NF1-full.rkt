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

(define (INCREASES pred)
  (fresh (_)
    (conde
      [(== `(,_ . "produces") pred)]
      [(== `(,_ . "positively_regulates") pred)])))

(displayln "preds for X ANY-PRED NF1")
(time (rem-dups
        (run* (q)
            (fresh (edge eid tacrine-details subject object pred eprops db)
              (== `(,eid ,subject ,object ,pred . ,eprops) edge)
              (~name-concepto "NF1" `(,db . ,object))            
              (edgeo `(,db . ,edge))
              (== pred q)))))
;; =>
'((0 . "interacts_with")
  (3 . "part_of")
  (5 . "negatively_regulates")
  (6 . "coexists_with")
  (9 . "affects")
  (10 . "produces")
  (0 . "directly_interacts_with")
  (13 . "positively_regulates")
  (3 . "interacts_with")
  (6 . "part_of")
  (3 . "regulates")
  (10 . "coexists_with")
  (16 . "location_of")
  (17 . "produces")
  (18 . "positively_regulates")
  (19 . "negatively_regulates"))

#|
(time (pretty-print
       (run* (q)
         (fresh (edge name db)
           (fresh (subject scid scui sname sdetails
                           object ocid ocui oname odetails
                           eid pid pred eprops)
             (== `(,scid ,scui ,sname . ,sdetails) subject)
             (== `(,ocid ,ocui ,oname . ,odetails) object)
             (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
             (~name-concepto "NF1" `(,db . ,object))
             (== `(,db . ,object) q))))))

(displayln "X ANY-PRED NF1")
(time (pretty-print
       (run* (q)
         (fresh (edge name db)
           (fresh (subject scid scui sname sdetails
                           object ocid ocui oname odetails
                           eid pid pred eprops)
             (== `(,scid ,scui ,sname . ,sdetails) subject)
             (== `(,ocid ,ocui ,oname . ,odetails) object)
             (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
             (~name-concepto "NF1" `(,db . ,object))
             (== `(,sname ,pred ,oname) q)
             (edgeo `(,db . ,edge)))))))

(displayln "X ANY-PRED HFNF1A")
(time (pretty-print
       (run* (q)
         (fresh (edge name db)
           (fresh (subject scid scui sname sdetails
                           object ocid ocui oname odetails
                           eid pid pred eprops)
             (== `(,scid ,scui ,sname . ,sdetails) subject)
             (== `(,ocid ,ocui ,oname . ,odetails) object)
             (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
             (== `(,db . ,object)
                 `(monarch
                   728
                   "HGNC:11621"
                   "HNF1A"
                   (1 . "['gene', 'sequence feature']")
                   ("iri" . "http://identifiers.org/hgnc/HGNC:11621")
                   ("clique"
                    .
                    "['HGNC:11621', 'NCBIGene:6927', 'Orphanet:158583', 'OMIM:142410', 'ENSEMBL:ENSG00000135100', 'ENSEMBL:ENSP00000443964', 'ENSEMBL:ENSP00000439721', 'ENSEMBL:ENSP00000443112', 'ENSEMBL:ENSP00000445445', 'ENSEMBL:ENSP00000481967', 'ENSEMBL:ENSP00000257555', 'UniProtKB:P20823', 'ENSEMBL:ENSP00000440361', 'ENSEMBL:ENSP00000483994', 'ENSEMBL:ENSP00000438565', 'ENSEMBL:ENSP00000438804', 'ENSEMBL:ENSP00000453965', 'ENSEMBL:ENSP00000476181']")
                   ("http://purl.org/dc/elements/1.1/description" . "HNF1 homeobox A")
                   ("http://www.w3.org/2000/01/rdf-schema#label" . "HNF1A")
                   ("id" . "HGNC:11621")
                   ("label" . "HNF1A")
                   ("https://monarchinitiative.org/MONARCH_cliqueLeader" . "True")
                   ("labels"
                    .
                    "['gene', 'sequence_feature', 'Class', 'cliqueLeader', 'Node']")))
             (== `(,sname ,pred ,oname) q)
             (edgeo `(,db . ,edge)))))))

(displayln "X ANY-PRED RNF183")
(time (pretty-print
       (run* (q)
         (fresh (edge name db)
           (fresh (subject scid scui sname sdetails
                           object ocid ocui oname odetails
                           eid pid pred eprops)
             (== `(,scid ,scui ,sname . ,sdetails) subject)
             (== `(,ocid ,ocui ,oname . ,odetails) object)
             (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
             (== `(,db . ,object)
                 `(rtx
                   18446
                   "UniProtKB:Q96D59"
                   "RNF183"
                   (1 . "protein")
                   ("symbol" . "RNF183")
                   ("expanded" . "True")
                   ("rtx_name" . "Q96D59")
                   ("description" . "None")
                   ("id" . "UniProtKB:Q96D59")
                   ("accession" . "Q96D59")
                   ("UUID" . "de83c902-5907-11e8-95d6-060473434358")
                   ("uri" . "http://identifiers.org/uniprot/Q96D59")
                   ("seed_node_uuid" . "dbf1734c-5907-11e8-95d6-060473434358")
                   ("labels" . "['protein', 'Base']")))
             (== `(,sname ,pred ,oname) q)
             (edgeo `(,db . ,edge)))))))

(displayln "X ANY-PRED HNF1A")
(time (pretty-print
       (run* (q)
         (fresh (edge name db)
           (fresh (subject scid scui sname sdetails
                           object ocid ocui oname odetails
                           eid pid pred eprops)
             (== `(,scid ,scui ,sname . ,sdetails) subject)
             (== `(,ocid ,ocui ,oname . ,odetails) object)
             (== `(,eid ,subject ,object (,pid . ,pred) . ,eprops) edge)
             (== `(,db . ,object)
                 `(scigraph
                   728
                   "HGNC:11621"
                   "HNF1A"
                   (1 . "['gene', 'sequence feature']")
                   ("iri" . "http://identifiers.org/hgnc/HGNC:11621")
                   ("clique"
                    .
                    "['HGNC:11621', 'NCBIGene:6927', 'Orphanet:158583', 'OMIM:142410', 'ENSEMBL:ENSG00000135100', 'ENSEMBL:ENSP00000443964', 'ENSEMBL:ENSP00000439721', 'ENSEMBL:ENSP00000443112', 'ENSEMBL:ENSP00000445445', 'ENSEMBL:ENSP00000481967', 'ENSEMBL:ENSP00000257555', 'UniProtKB:P20823', 'ENSEMBL:ENSP00000440361', 'ENSEMBL:ENSP00000483994', 'ENSEMBL:ENSP00000438565', 'ENSEMBL:ENSP00000438804', 'ENSEMBL:ENSP00000453965', 'ENSEMBL:ENSP00000476181']")
                   ("http://purl.org/dc/elements/1.1/description" . "HNF1 homeobox A")
                   ("http://www.w3.org/2000/01/rdf-schema#label" . "HNF1A")
                   ("id" . "HGNC:11621")
                   ("label" . "HNF1A")
                   ("https://monarchinitiative.org/MONARCH_cliqueLeader" . "True")
                   ("labels" . "['gene', 'Node']")))
             (== `(,sname ,pred ,oname) q)
             (edgeo `(,db . ,edge)))))))

|#

(newline)
(displayln "X INCREASES NF1:")
(time (pretty-print
        (run* (edge)
          (fresh (eid tacrine-details subject object pred eprops db)
            (== `(,eid ,subject ,object ,pred . ,eprops) edge)
            (~name-concepto "NF1" `(,db . ,object))
            (INCREASES pred)
            (edgeo `(,db . ,edge))))))

(time (pretty-print
        (run* (q)
          (fresh (edge eid tacrine-details subject object pred eprops db)
            (== `(,eid ,subject ,object ,pred . ,eprops) edge)
            ;(~name-concepto "congenital cataract" `(,db . ,object))
            (== `(81155
                  "HP:0000519"
                  "Congenital cataract"
                  (5 . "phenotypic_feature")
                  ("expanded" . "True")
                  ("rtx_name" . "HP:0000519")
                  ("description" . "A congenital cataract.")
                  ("id" . "HP:0000519")
                  ("accession" . "0000519")
                  ("UUID" . "ec2d2c9c-5907-11e8-95d6-060473434358")
                  ("uri" . "http://purl.obolibrary.org/obo/HP_0000519")
                  ("seed_node_uuid" . "dbf1734c-5907-11e8-95d6-060473434358")
                  ("labels" . "['Base', 'phenotypic_feature']"))
                object)
            (edgeo `(,db . ,edge))
            (== `(,db . ,edge) q)
            ))))
