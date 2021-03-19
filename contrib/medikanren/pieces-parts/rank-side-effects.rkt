#lang racket
(require
  "../../../medikanren/pieces-parts/query.rkt")

#|
takes: a drug curie as a string (a-drug), a list of predicates of interest (predicates)
Does query X->O where X is a-drug and the edges are filtered by predicates
returns: a list of the curies O from the X->O query
|#
(define (filter-edges a-drug predicates)
  (define q
    (time
     (query/graph
      ((X a-drug)
       (O #f))
      ((X->O predicates))
      (X X->O O))))
  (curies/query q 'O))

;;predicates that indicate a side effect
(define side-effect-preds (list "causes"
                                "contributes_to"
                                "contraindicated_for"))

#|
takes: a disease curie as a string (disease-curie), a set of patient symptoms/phenotypes (empty set if none) (patient-phenos)
Ranks the drugs related to the disease-curie by the nature and number of side effects they cause
returns: A list of 3 lists. These lists contain all of the drug curies resulting from a drug->disease-curie query sorted as follows:
         The last list contains the drugs that aggravate any pre-existing patient symptom.
         The middle list contains the drugs that would further aggravate any symptoms caused by the disease (disease-curie)
         The first list contains the rest of the drugs not in the 2nd or 3rd list that have a drug->disease (disease-curie) relationship.
         The drugs within all three lists are sorted (least to most) by the number of side effects they have.
|#
(define (rank-drugs-related-to disease-curie patient-phenos)
  ;;D->O, a query where D is any drug that has an edge to the disease-curie, O.
  (define qtreats (query/graph
                   ((D drug)
                    (O disease-curie))
                   ((D->O #f))
                   (D D->O O)))
  (define drugs (curies/query qtreats 'D))
  ;;a query to get all of the symptoms/phenotypes of the given disease-curie, + the disease-curie itself
  (define qphenos (query/graph
                   ((S disease-curie)
                    (P phenotype))
                   ((S->P #f))
                   (S S->P P)))
  (define disease-phenos (set-add (list->set (map curie-synonyms/names (curies/query qphenos 'P))) disease-curie))
  
  ;;a hash of all the drugs to a set of their side effects
  (define drug->side-effects (make-hash))
  (for-each
   (lambda (d)
     (hash-set! drug->side-effects d (list->set (filter-edges d side-effect-preds)))
     )
   drugs)
  ;;determines whether a given drug (drg) exacerbates any phenotype in a set of phenotypes (ph), returns a bool
  (define (exacerbates? drg ph)
    (not (set-empty? (set-intersect (hash-ref drug->side-effects drg) ph))))
  
  (define exacerbates-patient (mutable-set))
  (define exacerbates-disease (mutable-set))
  (define top-rank (mutable-set))
  (for-each
   (lambda (d)
     (cond
       [(exacerbates? d patient-phenos) (set-add! exacerbates-patient d)]
       [(exacerbates? d disease-phenos) (set-add! exacerbates-disease d)]
       [else (set-add! top-rank d)]
       )
     )
   drugs)
  ;;function to sort by number of side effects
  (define (rank-num-side-effects drgs)
    (sort (set->list drgs)
          >
          #:key (lambda (d) (set-count (hash-ref drug->side-effects d))))
    )
  (list (rank-num-side-effects top-rank) (rank-num-side-effects exacerbates-disease) (rank-num-side-effects exacerbates-patient))
  )
#|Examples
Hemophilia A, no pre-existing patient symptoms
(rank-drugs-related-to "MONDO:0010602" (set))
Hemophilia A, patient also has liver disease and anemia
(rank-drugs-related-to "MONDO:0010602" (set "MONDO:0005154" "HP:0001903"))
|#