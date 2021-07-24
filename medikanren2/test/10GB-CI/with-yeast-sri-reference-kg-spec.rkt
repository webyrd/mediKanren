#lang racket
(require "../../base.rkt"
         "../../db/yeast-sri-reference-kg-0.3.0.rkt"
         chk)


(define uri-scn4a "ENSEMBL:ENSBTAG00000004770")

(chk
 (#:= (run* (p o)
            (fresh (id)
                   (== id uri-scn4a)
                   (membero p '("in_taxon" "description"))
                   (cprop id p o)))
  '(("in_taxon" "NCBITaxon:9913|NCBITaxon:9913")
    ("description" "Sodium channel protein"))))


(define uri-ion-transport "GO:0034765")

(chk
 (#:= (run* (p o)
            (fresh (id)
                   (== id uri-ion-transport)
                   (membero p '("iri" "name"))
                   (cprop id p o)))
  '(("iri" "http://purl.obolibrary.org/obo/GO_0034765")
    ("name" "regulation of ion transmembrane transport"))))

(define (mk2-edgedetail-from-uris dbedge dbeprop uri-src uri-dst)
  (map (lambda (p) (apply cons p))
       (run* (ep-pred ep-obj)
                 (fresh (e-id ep-sub)
                            (dbedge e-id uri-src uri-dst)
                            (dbeprop ep-sub ep-pred ep-obj)
                            (== e-id ep-sub)))))

(define (check-expected-field expected actual fd)
;  (printf "check-expected-field ~a ~a\n" (dict-ref expected fd) (dict-ref actual fd))
  (equal? (dict-ref expected fd) (dict-ref actual fd)))

(chk
 (#:do (define expected '(("edge_label" . "biolink:has_part")
                          ("provided_by" . "monarch-ontologies")
                          ("relation" . "BFO:0000051"))))
 (#:do (define actual
         (mk2-edgedetail-from-uris edge eprop "UBERON:4000115" "CHEBI:52255")))
 (#:t (check-expected-field expected actual "edge_label"))
 (#:t (check-expected-field expected actual "provided_by"))
 (#:t (check-expected-field expected actual "relation")))


#|
  Expected results for mk2-edgedetail-from-uris tests were corroborated
  with sri-reference-kg-0.2.0 and this medikanren1 code.  Note that due to
  0.2.0 to 0.3.0 data versioning, results are not identical.

(require (prefix-in mk1: "../../medikanren/common.rkt"))

(define (ids-from-uri dbname uri)
  (mk1:run*
   (cid)
   (mk1:fresh (concept cui^ name catid cat props)
              (mk1:~cui-concepto uri concept)
              (mk1:==
               concept
               `(,dbname ,cid ,cui^ ,name (,catid . ,cat) ,props)))))

(define (mk1-edgedetail-from-uris db uri-src uri-dst)
  (let ((ids (ids-from-uri db uri-src)))
    (append-map
     (lambda (scid0)
       (mk1:run*
        (eprops)
        (mk1:fresh (edge dbname eid
                           scid scui sname scatid scat sprops
                           ocid ocui oname ocatid ocat oprops
                           pid pred #;eprops
                           )
                   (mk1:== edge `(,dbname ,eid
                                          (,scid ,scui ,sname (,scatid . ,scat) ,sprops)
                                          (,ocid ,ocui ,oname (,ocatid . ,ocat) ,oprops)
                                          (,pid . ,pred) ,eprops))
                   (mk1:== dbname db)
                   (mk1:== scid scid0)
                   (mk1:== ocui uri-dst)
                   (mk1:edgeo edge))))
     ids)))

(pretty-write
 (mk1-edgedetail-from-uris 'sri-reference-kg-0.2.0 "UBERON:4000115" "CHEBI:52255"))
#;((("relation" . "BFO:0000051") ("provided_by" . "Monarch Ontologies")))

|#