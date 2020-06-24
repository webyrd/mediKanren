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
  (filter-edges a-drug (or "causes" "contributes_to")))

(define (phenotypes a-protein)
  (filter-edges a-protein "has_phenotype"))

(define (counterindications a-drug)
  (filter-edges a-drug "contraindicated_for"))

(define (indications a-drug)
  (filter-edges a-drug "indicated_for"))

#;
(side-effects imatinib)
#;
(counterindications imatinib)

; dexamethasone
(define dex-list (side-effects "CUI:C0011777"))

(define (get-cars a-list)
  (if (null? a-list) '()
      (append (car a-list) (get-cars (cdr a-list)))))

(define all-pairs-effects-dex (get-cars dex-list))

(define (get-cdr a-list)
  (if (null? a-list) '()
      (cons (cdr (car a-list)) (get-cdr (cdr a-list)))))

(define all-side-effects-dex (get-cdr all-pairs-effects-dex))

(define (list-intersect-2 lst1 lst2)
  (set->list
   (set-intersect (list->set lst1)
                  (list->set lst2))))

(define rand-list (phenotypes "UniProtKB:Q4LDG9"))
(define all-pairs-effects-rand (get-cars rand-list))
(define all-side-effects-rand (get-cdr all-pairs-effects-rand))

(list-intersect-2 all-side-effects-rand all-side-effects-dex)