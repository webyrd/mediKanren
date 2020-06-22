(define (filter-edges a-drug an-edge)
  (define q
    (time
     (query/graph
      ((X a-drug)
       (O #f))
      ((X->O #f (lambda (e) (string=? an-edge
                                      (cdr (list-ref e 4))))))
      (X X->O O))))
  (map curie-synonyms/names (curies/query q 'O)))

(define (side-effects a-drug)
  (filter-edges a-drug "causes_or_contributes_to"))

(define (counterindications a-drug)
  (filter-edges a-drug "causes_or_contributes_to"))

#;
(side-effects imatinib)
#;
(counterindications imatinib)
