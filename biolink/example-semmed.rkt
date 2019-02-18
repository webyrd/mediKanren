#lang racket

(provide
  (all-from-out "mk-db.rkt")
  (all-defined-out)
  )

(require
  "mk-db.rkt"
  )

(define semmed (time (make-db "data/semmed")))

;(displayln "categories:")
;(pretty-print (run* (c) (db:categoryo semmed c)))

;(newline)
;(displayln "predicates:")
;(pretty-print (run* (p) (db:predicateo semmed p)))

;(newline)
;(displayln "some concepts:")
;(pretty-print (run 10 (c) (db:concepto semmed c)))

;(newline)
;(displayln "some edges:")
;(pretty-print (run 10 (e) (db:edgeo semmed e)))

(newline)
(displayln "fuzzy search:")
(time (pretty-print (run* (c) (db:~name-concepto semmed "imatin" c))))

(newline)
(displayln "case-sensitive search:")
(time (pretty-print (run* (c) (db:~name-concepto/options #t "" "" semmed "imatin" c))))

(newline)
(displayln "ignore-spaces search:")
(time (pretty-print (run* (c) (db:~name-concepto/options #f " " "" semmed "imatinib400" c))))

(newline)
(displayln "exact-word search (empty result):")
(time (pretty-print (run* (c) (db:~name-concepto/options #f "" chars:split-typical semmed "imatin" c))))

(newline)
(displayln "exact-word search:")
(time (pretty-print (run* (c)
                      (db:~name-concepto/options #f "" chars:split-typical semmed "imatinib" c)
                      (db:~name-concepto/options #f "" chars:split-typical semmed "400" c))))

(newline)
(displayln "exact-word search, order swapped:")
(time (pretty-print (run* (c)
                      (db:~name-concepto/options #f "" chars:split-typical semmed "400" c)
                      (db:~name-concepto/options #f "" chars:split-typical semmed "imatinib" c))))

;(newline)
;(displayln "exact-word search:")
;(time (pretty-print (run* (c) (db:~name-concepto/options #f "" "" semmed "imatin" c))))

;(displayln "associating pubmed ids with edge ids:")
;(time (pretty-print (run 10 (pmid eid) (db:pmid-eido semmed pmid eid))))
;(time (pretty-print (run*        (eid) (db:pmid-eido semmed "1000085" eid))))
;(time (pretty-print (run*        (eid) (db:pmid-eido semmed "10000" eid))))
;(time (pretty-print (run*        (eid) (db:pmid-eido semmed "999999" eid))))
