#lang racket
(provide
  (all-from-out "../../common.rkt")
  (all-defined-out))
(require "../../common.rkt")

(displayln "all categories:")
(pretty-print (run* (c) (categoryo c)))

(newline)
(displayln "all predicates:")
(pretty-print (run* (p) (predicateo p)))

(newline)
(displayln "some concepts:")
(pretty-print (run 10 (c) (concepto c)))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (edgeo e)))

(newline)
(displayln "fuzzy name search:")
(time (pretty-print
        ;; Match names that include all listed fragments.
        (run* (c) (~name*-concepto (list "imatin" "400") c))))

(newline)
(displayln "CURIE search:")
(time (pretty-print
        (run* (c)
          (~cui*-concepto
            ;; Match CURIEs that match any of the listed fragments.
            (list "UMLS:C0004096" "DOID:2841" "HP:0002099" "MONDO:0004979")
            c))))

(newline)
(displayln "xref search:")
(time (pretty-print
        (run* (xref c)
          (membero xref '("DOID:2841" "HP:0002099" "MONDO:0004979"))
          (xref-concepto xref c))))

(newline)
(displayln "finding pubmed ids and associated edges:")
(time (pretty-print (run 3 (pmid e) (pmid-edgeo pmid      e))))
(displayln "associating specific pubmed ids with edges:")
(time (pretty-print (run*       (e) (pmid-edgeo "1000085" e))))
(time (pretty-print (run*       (e) (pmid-edgeo "1000018" e))))
(time (pretty-print (run*       (e) (pmid-edgeo "10000"   e))))
(time (pretty-print (run*       (e) (pmid-edgeo "999999"  e))))
