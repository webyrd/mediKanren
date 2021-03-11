#lang racket
(require
  "../../medikanren/pieces-parts/mk-db.rkt"
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
   ("umls_type_label" . "['Organic Chemical']")
   ("xrefs" . "['MESH:C098212']")
   ("id" . "UMLS:C0386973")
   ("umls_type" . "['T109']")
   ("labels" . "['chemical_substance']"))
  (70919
   "UMLS:C0295380"
   "2-hydroxytacrine"
   (5 . "chemical_substance")
   ("umls_type_label" . "['Pharmacologic Substance', 'Organic Chemical']")
   ("xrefs" . "['MESH:C092548']")
   ("id" . "UMLS:C0295380")
   ("umls_type" . "['T121', 'T109']")
   ("labels" . "['chemical_substance']")))


(newline)
(displayln "fuzzy search for tacrine, displayed with something similar to the old format:")
(time (pretty-print (map
                      (lambda (e)
                        (match e
                          [`(,id ,cui ,name ,ignore2 . ,props)
                           (let ((type-p (assoc "umls_type_label" props)))
                             (let ((type (if type-p
                                             (cdr type-p)
                                             'no-type-found)))
                               (list id cui name type)))]))
                      (run* (c) (db:~name-concepto semmed "tacrine" c)))))
;; =>
'((35887 "UMLS:C0386973" "6-chlorotacrine" "['Organic Chemical']")
  (70919 "UMLS:C0295380" "2-hydroxytacrine" "['Pharmacologic Substance', 'Organic Chemical']")
  (75842 "UMLS:C0039245" "Tacrine" "['Pharmacologic Substance', 'Organic Chemical']")
  (98097 "UMLS:C1435294" "N-butyramide-tacrine" "['Pharmacologic Substance', 'Organic Chemical']")
  (98688 "UMLS:C0771182" "Tacrine Hydrochloride" "['Pharmacologic Substance', 'Organic Chemical']")
  (101040 "UMLS:C0659809" "N-methyltacrine" "['Organic Chemical']")
  (123792 "UMLS:C0295379" "4-hydroxytacrine" "['Pharmacologic Substance', 'Organic Chemical']")
  (154109 "UMLS:C0099894" "7-methoxytacrine" "['Pharmacologic Substance', 'Organic Chemical']"))

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
(time (pretty-print
        (run* (edge)
          (fresh (eid subject tacrine-details pred eprops)
            (== `(,eid ,subject (75842 . ,tacrine-details) ,pred . ,eprops) edge)
            (== `(0 . "interacts_with") pred)
            (db:edgeo semmed edge)))))

(newline)
(displayln "tacrine treats X:")
(time (pretty-print
        (run* (edge)
          (fresh (eid tacrine-details object pred eprops)
            (== `(,eid (75842 . ,tacrine-details) ,object ,pred . ,eprops) edge)
            (== `(1 . "treats") pred)
            (db:edgeo semmed edge)))))

(define (DECREASES pred)
  (conde
    [(== `(1 . "treats") pred)]
    [(== `(14 . "prevents") pred)]))

(newline)
(displayln "tacrine DECREASES X:")
(time (pretty-print
        (run* (edge)
          (fresh (eid tacrine-details object pred eprops)
            (== `(,eid (75842 . ,tacrine-details) ,object ,pred . ,eprops) edge)
            (DECREASES pred)
            (db:edgeo semmed edge)))))
