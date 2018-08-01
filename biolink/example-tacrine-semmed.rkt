#lang racket
(require
  "mk-db.rkt"
  )

(define semmed (make-db "data/semmed"))


(displayln "------- tacrine examples, using semmed ---------")

(newline)
(displayln "fuzzy search for tacrine:")
(time (pretty-print (run* (c) (db:~name-concepto semmed "tacrine" c))))

(newline)
(displayln "first two responses for fuzzy search for tacrine:")
(time (pretty-print (run 2 (c) (db:~name-concepto semmed "tacrine" c))))
;; =>
'((35887
   "UMLS:C0386973"
   "6-chlorotacrine"
   (5 . "chemical_substance")
   (("umls_type_label" . "['Organic Chemical']")
    ("xrefs" . "['MESH:C098212']")
    ("id" . "UMLS:C0386973")
    ("umls_type" . "['T109']")
    ("labels" . "['chemical_substance']")))
  (70919
   "UMLS:C0295380"
   "2-hydroxytacrine"
   (5 . "chemical_substance")
   (("umls_type_label" . "['Pharmacologic Substance', 'Organic Chemical']")
    ("xrefs" . "['MESH:C092548']")
    ("id" . "UMLS:C0295380")
    ("umls_type" . "['T121', 'T109']")
    ("labels" . "['chemical_substance']"))))


(newline)
(displayln "fuzzy search for tacrine, displayed with something similar to the old format:")
(time (pretty-print (map
                      (lambda (e)
                        (match e
                          [`(,ignore1 ,cui ,name ,ignore2 ,props)
                           (let ((type-p (assoc "umls_type_label" props)))
                             (let ((type (if type-p
                                             (cdr type-p)
                                             'no-type-found)))
                               (list cui name type)))]))
                      (run* (c) (db:~name-concepto semmed "tacrine" c)))))
;; =>
'(("UMLS:C0386973" "6-chlorotacrine" "['Organic Chemical']")
  ("UMLS:C0295380" "2-hydroxytacrine" "['Pharmacologic Substance', 'Organic Chemical']")
  ("UMLS:C0039245" "Tacrine" "['Pharmacologic Substance', 'Organic Chemical']")
  ("UMLS:C1435294" "N-butyramide-tacrine" "['Pharmacologic Substance', 'Organic Chemical']")
  ("UMLS:C0771182" "Tacrine Hydrochloride" "['Pharmacologic Substance', 'Organic Chemical']")
  ("UMLS:C0659809" "N-methyltacrine" "['Organic Chemical']")
  ("UMLS:C0295379" "4-hydroxytacrine" "['Pharmacologic Substance', 'Organic Chemical']")
  ("UMLS:C0099894" "7-methoxytacrine" "['Pharmacologic Substance', 'Organic Chemical']"))

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
(displayln "X interacts with tacrine:")
(time (pretty-print (run* (edge)
                      (fresh (scid scui sname scatid scat sprops
                                   ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                        (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                   (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                   (,pid . ,pred) . ,eprops) edge)
                        (== `(75842
                              "UMLS:C0039245"
                              "Tacrine"
                              (5 . "chemical_substance")
                              (("umls_type_label" . "['Pharmacologic Substance', 'Organic Chemical']")
                               ("xrefs"
                                .
                                "['MESH:D013619', 'CHV:0000012013', 'VANDF:4023896', 'LNC:MTHU024717', 'LNC:LP17860-5', 'NDFRT:N0000007004', 'RXNORM:10318', 'DRUGBANK:DB00382', 'SNOMEDCT_US:108494008', 'MTH:NOCODE', 'NDFRT:N0000021901', 'UNII:4VX7YNB537', 'ATC:N06DA01', 'NDDF:004378', 'MMSL:d03176', 'SNOMEDCT_US:373727000', 'INCHIKEY:YLJREFDVOIBQDA-UHFFFAOYSA-N', 'CHEMBL:CHEMBL95', 'CSP:0033-0786', 'NCI:C61961', 'MEDCIN:45792']")
                               ("id" . "UMLS:C0039245")
                               ("umls_type" . "['T121', 'T109']")
                               ("labels" . "['chemical_substance']")))
                            `(,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops)))
                        (== `(0 . "interacts_with") `(,pid . ,pred))
                        (db:edgeo semmed edge)))))

(newline)
(displayln "tacrine treats X:")
(time (pretty-print (run* (edge)
                      (fresh (scid scui sname scatid scat sprops
                                   ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                        (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                   (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                   (,pid . ,pred) . ,eprops) edge)
                        (== `(75842
                              "UMLS:C0039245"
                              "Tacrine"
                              (5 . "chemical_substance")
                              (("umls_type_label" . "['Pharmacologic Substance', 'Organic Chemical']")
                               ("xrefs"
                                .
                                "['MESH:D013619', 'CHV:0000012013', 'VANDF:4023896', 'LNC:MTHU024717', 'LNC:LP17860-5', 'NDFRT:N0000007004', 'RXNORM:10318', 'DRUGBANK:DB00382', 'SNOMEDCT_US:108494008', 'MTH:NOCODE', 'NDFRT:N0000021901', 'UNII:4VX7YNB537', 'ATC:N06DA01', 'NDDF:004378', 'MMSL:d03176', 'SNOMEDCT_US:373727000', 'INCHIKEY:YLJREFDVOIBQDA-UHFFFAOYSA-N', 'CHEMBL:CHEMBL95', 'CSP:0033-0786', 'NCI:C61961', 'MEDCIN:45792']")
                               ("id" . "UMLS:C0039245")
                               ("umls_type" . "['T121', 'T109']")
                               ("labels" . "['chemical_substance']")))
                            `(,scid . (,scui ,sname (,scatid . ,scat) . ,sprops)))
                        (== `(1 . "treats") `(,pid . ,pred))
                        (db:edgeo semmed edge)))))

(newline)
(displayln "tacrine DECREASES X:")
(time (pretty-print (let ((DECREASES (lambda (pred-info)
                                       (conde
                                         [(== `(1 . "treats") pred-info)]
                                         [(== `(14 . "prevents") pred-info)]))))
                      (run* (edge)
                        (fresh (scid scui sname scatid scat sprops
                                     ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                          (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                     (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                     (,pid . ,pred) . ,eprops) edge)
                          (== `(75842
                                "UMLS:C0039245"
                                "Tacrine"
                                (5 . "chemical_substance")
                                (("umls_type_label" . "['Pharmacologic Substance', 'Organic Chemical']")
                                 ("xrefs"
                                  .
                                  "['MESH:D013619', 'CHV:0000012013', 'VANDF:4023896', 'LNC:MTHU024717', 'LNC:LP17860-5', 'NDFRT:N0000007004', 'RXNORM:10318', 'DRUGBANK:DB00382', 'SNOMEDCT_US:108494008', 'MTH:NOCODE', 'NDFRT:N0000021901', 'UNII:4VX7YNB537', 'ATC:N06DA01', 'NDDF:004378', 'MMSL:d03176', 'SNOMEDCT_US:373727000', 'INCHIKEY:YLJREFDVOIBQDA-UHFFFAOYSA-N', 'CHEMBL:CHEMBL95', 'CSP:0033-0786', 'NCI:C61961', 'MEDCIN:45792']")
                                 ("id" . "UMLS:C0039245")
                                 ("umls_type" . "['T121', 'T109']")
                                 ("labels" . "['chemical_substance']")))
                              `(,scid . (,scui ,sname (,scatid . ,scat) . ,sprops)))
                          (DECREASES `(,pid . ,pred))
                          (db:edgeo semmed edge))))))
