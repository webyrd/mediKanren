#lang racket/base
(require "common.rkt" "db/semmed.rkt" racket/pretty)

(time (let ()
        (time (pretty-print
                (run 10 (k v)
                  (cprop "UMLS:C0000137" k v))))
        (newline)

        (time (pretty-print
                (run 10 (curie name)
                  (cprop curie "category" "chemical_substance")
                  (cprop curie "name" name))))
        (newline)

        (time (pretty-print
                (run 10 (curie1 predicate curie2)
                  (fresh (eid)
                    (eprop eid "edge_label" predicate)
                    (edge eid curie1 curie2)))))
        (newline)

        (time (pretty-print
                (run 10 (curie1 name1 curie2 name2)
                  (fresh (eid)
                    (eprop eid "edge_label" "negatively_regulates")
                    (edge eid curie1 curie2)
                    (cprop curie1 "name" name1)
                    (cprop curie2 "name" name2)))))
        (newline)

        (displayln "All concept property keys:")
        (time (pretty-print
                (run* (k)
                  (fresh (curie v)
                    (cprop curie k v)))))
        (newline)

        (displayln "All edge property keys:")
        (time (pretty-print
                (run* (k)
                  (fresh (eid v)
                    (eprop eid k v)))))
        (newline)

        (displayln "All concept categories:")
        (time (pretty-print
                (run* (category)
                  (fresh (curie)
                    (cprop curie "category" category)))))
        (newline)

        (displayln "All edge predicates:")
        (time (pretty-print
                (run* (predicate)
                  (fresh (eid)
                    (eprop eid "edge_label" predicate)))))
        (newline)

        (time (pretty-print
                (run* (k v)
                  (cprop "UMLS:C0520909" k v))))
        (newline)

        (time (pretty-print
                (run* (o cat name p)
                  (fresh (eid)
                    (edge eid "UMLS:C0520909" o)
                    (cprop o "category" cat)
                    (cprop o "name" name)
                    (eprop eid "edge_label" p)))))
        (newline)
        ))
