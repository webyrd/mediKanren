(define (foo sym curie)
  (define gene (concept/curie curie))
  (define X-drug (concept/category drug))
  (define X-downregs (edge/predicate negatively-regulates gene X-drug))

  (run!)
  (map list
       `(,sym ,curie X-drug X-downregs)
       (map summarize (list gene X-drug X-downregs))))

(define alms1 (foo 'ALMS1 "HGNC:428"))

(define rpgrip1l (foo 'RPGRIP1L "HGNC:29168"))

(define inpp5e (foo 'INPP5E "HGNC:21474"))

(define nphp3 (foo 'NPHP3 "HGNC:7907"))

(define kdm1a (foo 'KDM1A "HGNC:29079"))
