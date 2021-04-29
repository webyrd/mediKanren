#lang racket
(require chk)
(require "../../db/yeast-sri-reference-kg-0.3.0.rkt")
(require "../../dbk/mk.rkt")


(define uri-scn4a "ENSEMBL:ENSBTAG00000004770")

(run 1 (id p o)
     (== id curie-scn4a)
     (nodes id p o))

(chk
 (#:= (run* (p o)
            (fresh (id)
                   (== id uri-scn4a)
                   (membero p '("in_taxon" "description"))
                   (nodes id p o)))
  '(("in_taxon" "NCBITaxon:9913|NCBITaxon:9913")
    ("description" "Sodium channel protein"))))


(define uri-ion-transport "GO:0034765")

(chk
 (#:= (run* (p o)
            (fresh (id)
                   (== id uri-ion-transport)
                   (membero p '("iri" "name"))
                   (nodes id p o)))
  '(("iri" "http://purl.obolibrary.org/obo/GO_0034765")
    ("name" "regulation of ion transmembrane transport"))))


