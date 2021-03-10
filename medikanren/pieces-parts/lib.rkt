(define (filter-edges a-drug predicates)
  (define q
    (time
     (query/graph
      ((X a-drug)
       (O #f))
      ((X->O predicates))
      (X X->O O))))
  (map curie-synonyms/names (curies/query q 'O)))

(define (side-effects a-drug)
  (filter-edges a-drug (list "causes" "contributes_to")))

(define (counterindications a-drug)
  (filter-edges a-drug (list "contraindicated_with_disease")))

;;(side-effects imatinib)
;;(counterindications imatinib)

(define (all-predicates start)
(define q
  (time
   (query/graph
    ((X start)
     (O #f))
    ((X->O #f))
    (X X->O O))))
(sort (remove-duplicates (map (lambda (e) (cdr (list-ref e 4))) (edges/query q 'X->O))) string<=?)
)
;;(all-predicates imatinib)
