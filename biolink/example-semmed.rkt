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
(time (pretty-print (run* (c) (db:~name-concepto/options
                                #t "" "" semmed (list "imatin") c))))

(newline)
(displayln "case-insensitive search:")
(time (pretty-print (run* (c) (db:~name-concepto/options
                                #t "" "" semmed (list "imatin") c))))

(newline)
(displayln "ignore-spaces search:")
(time (pretty-print (run* (c) (db:~name-concepto/options
                                #f " " "" semmed (list "imatinib400") c))))

(newline)
(displayln "exact-word search (empty result):")
(time (pretty-print (run* (c) (db:~name-concepto/options
                                #f "" chars:split-typical semmed (list "imatin") c))))

(newline)
(displayln "exact-word search:")
(time (pretty-print (run* (c) (db:~name-concepto/options
                                #f "" chars:split-typical semmed (list "imatinib" "400") c))))

(newline)
(displayln "exact-word search, order swapped:")
(time (pretty-print (run* (c) (db:~name-concepto/options
                                #f "" chars:split-typical semmed (list "400" "imatinib") c))))

(newline)
(displayln "inexact-word search:")
(time (pretty-print (run* (c) (db:~name-concepto/options
                                #f "" "" semmed (list "imatini" "400") c))))

(newline)
(displayln "inexact-word search, order swapped:")
(time (pretty-print (run* (c) (db:~name-concepto/options
                                #f "" "" semmed (list "400" "imatini") c))))


(newline)
(displayln "testing Racket's string-split:")
(string-split "  foo    bar  " #px"\\s+")

;(newline)
;(displayln "exact-word search:")
;(time (pretty-print (run* (c) (db:~name-concepto/options #f "" "" semmed "imatin" c))))

;(displayln "associating pubmed ids with edge ids:")
;(time (pretty-print (run 10 (pmid eid) (db:pmid-eido semmed pmid eid))))
;(time (pretty-print (run*        (eid) (db:pmid-eido semmed "1000085" eid))))
;(time (pretty-print (run*        (eid) (db:pmid-eido semmed "10000" eid))))
;(time (pretty-print (run*        (eid) (db:pmid-eido semmed "999999" eid))))
