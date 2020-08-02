#lang racket
(require "query.rkt")

(define spikes '("UniProtKB:P0DTC2" "UniProtKB:P59594"))
                 
(define spike-covid19 "UniProtKB:P0DTC2")

(define spike-sars "UniProtKB:P59594")

(define ace2 "UMLS:C1422064")

(define covid19 "MONDO:0100096")

(define sars "CUI:C1175743")

(define mers "CUI:C3698360")

#|Possibly interesting preds
"biolink:interacts_with"
"biolink:target_has_causal_agent"
"biolink:positively_regulates"
"biolink:negatively_regulates"
"biolink:results_in_development_of"
"biolink:overlaps"
"biolink:results_in"
"biolink:has_quality"
"biolink:contributes_to_morphology_of"
"biolink:produced_by"
"biolink:immediately_causally_upstream_of"
"biolink:functionally_related_to"
"biolink:produces"
"biolink:precedes"
"biolink:preceded_by"
"biolink:protects"
"biolink:disease_has_feature"
"biolink:disease_has_basis_in_development_of"
"biolink:predisposes_towards"
"biolink:disease_shares_features_of"
"biolink:disease_causes_feature"
"biolink:disease_disrupts"
"biolink:disease_causes_dysfunction_of"
"biolink:part_of_progression_of_disease"
"biolink:disease_triggers"
|#

(define (member? elem lst)
  (not (not (member elem lst))))

(define (unwrap lst)
  (if (null? lst) lst
      (append (car lst) (unwrap (cdr lst)))))

;;TODO: implement more efficiently
#|
(define (find-dups l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (if (member? (car l1) l2) (cons (car l1) (find-dups (cdr l1) l2))
          (find-dups (cdr l1) l2))))
|#
(define (find-dups l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (set-intersect l1 l2)))

(define (predicate e)
  (cdr (list-ref e 4)))

(define (sort-pubs pubs)
  (sort
   (hash-map pubs (lambda (k v) (cons k v)))
   >
   #:key (lambda (x) (set-count (cdr x)))))

(define (sort-by-length h)
  (sort
   (hash-map h (lambda (k v) (cons k v)))
   >
   #:key (lambda (x) (length (cdr x)))))

(define (sort-count c)
  (sort
   (hash-map c (lambda (k v) (cons k v)))
   >
   #:key (lambda (x) (cdr x))))


(define (cluster-concepts-by-pubs c result-filter)
  (define q1 (query/graph
              ((C c)
               (P #f))
              ((C->P  '("biolink:related_to")))
              (C C->P P)))
  (define related-pubs (curies/query q1 'P))
  (define publications (make-hash))
  (define (step2 p)
    (printf ".\n")
    (define q2 (query/graph
                ((C result-filter)
                 (P p))
                ((C->P  '("biolink:related_to")))
                (C C->P P)))
    (define related-concepts (curies/query q2 'C))
    (for-each
     (lambda (c) (hash-update! publications c (lambda (s) (set-add s p)) (set)))
     related-concepts)
    )
  (for-each step2 related-pubs)
  (sort-pubs publications)
  )


(define co-occur-x (make-hash))
#|
(for-each
 (lambda (g)
   (define qgenes (query/graph
                   ((X #f)
                    (G g))
                   ((X->G #f))
                   (X X->G G)))
   (define g-subj (curies/query qgenes 'X))
   (hash-set! co-occur-x g (find-dups g-subj sprot-subj))
   )
 high-co
 )
|#
(sort-by-length co-occur-x)

#|
high-co - highest in spike-covid19->gene cluster
dir-related - gene-cluster->spike-covid19 that are directly related
|#
(define covid-spike-gene-cluster (cluster-concepts-by-pubs spike-covid19 gene))
(define top100 (take (map car (sort-pubs covid-spike-gene-cluster)) 100))
(define cluster-curies (map car (sort-pubs covid-spike-gene-cluster)))

(define qcovid-spike1 (query/graph
              ((X #f)
               (S spike-covid19))
              ((X->S #f))
              (X X->S S)))

(define qcovid-spike2 (query/graph
              ((S spike-covid19)
               (X #f))
              ((S->X #f))
              (S S->X X)))

(define covid-spike-subj (set-union (curies/query qcovid-spike1 'X) (curies/query qcovid-spike2 'X)))

(define co-occurs (find-dups (map car (unwrap (map curie-synonyms/names covid-spike-subj))) cluster-curies))

;;all biolink:interacts_with -> spike-covid19
(define dir-related (map car (unwrap (map curie-synonyms/names co-occurs))))

(define dir-related-es (map
                        (lambda (g)
                          (define qpreds (query/graph
                                          ((X g)
                                           (S spike-covid19))
                                          ((X->S #f))
                                          (X X->S S)))
                          (summarize-edge (edges/query qpreds 'X->S))
                          )
                        dir-related
                        )
  )

(define G->M->X (make-hash))

(define go-regulators '("positively_regulates"
                       "negatively_regulates"
                       ;;"regulates"
                       "subclass_of"
                       ;;"involved_in"
                       )
  )

(for-each
 (lambda (g)
   (define q-2-hop (query/graph
                    ((G g)
                     (M #f)
                     (X #f ))
                    ((G->M '("involved_in"))
                     (M->X go-regulators))
                    (G G->M M)
                    (M M->X X)))
   (hash-set! G->M->X g (summarize-edge (edges/query q-2-hop 'M->X)))
   )
 co-occurs
 )

(define q-2-hop-test (query/graph
                      ((G "CUI:C1422064")
                       (M #f)
                       (X #f ))
                      ((G->M '("involved_in"))
                       (M->X go-regulators))
                      (G G->M M)
                      (M M->X X)))
;;take X and do S -involved_in-> X
;;disease -> gene -> go pathway

(define g->m->x (sort-by-length G->M->X))

(define qdrug1 (query/graph
                ((D drug)
                 (G "CUI:C1422064"))
                ((D->G #f))
                (D D->G G)))

(define drugs->ace2 (curies/query qdrug1 'D))

(define ace2-drug-cluster (cluster-concepts-by-pubs "CUI:C1422064" drug))

(define dir-related-drugs (find-dups drugs->ace2 (map car ace2-drug-cluster)))
(map
 (lambda (d)
   (define q (query/graph
              ((D d)
               (G "CUI:C1422064"))
              ((D->G #f))
              (D D->G G)))
   (summarize-edge (edges/query q 'D->G))
   )
 dir-related-drugs
 )

(define qtest (query/graph
               ((A "CUI:C1422064")
                (P disease))
               ((A->P #f))
               (A A->P P)))

;;pos/neg regulates
;;stimulates
;;inhibits

;;(car (car g->m->x))
;;(car (list-ref (car (cdar g->m->x)) 4))
;;("GO:0051239" . "regulation of multicellular organismal process")

;;cluster based on multiple specific curies instead of just one
#|
sars disease curie cluster test
|#
(define sars->gene-cluster (cluster-concepts-by-pubs sars gene))



#|
(define (related-count c result-filter)
  (define q1 (query/graph
              ((C c)
               (P #f))
              ((C->P  '("biolink:related_to")))
              (C C->P P)))
  (define related-pubs (curies/query q1 'P))
  (define counter (make-hash))
  (define (step2 p)
    (printf ".\n")
    (define q2 (query/graph
                ((C result-filter)
                 (P p))
                ((C->P  '("biolink:related_to")))
                (C C->P P)))
    (define related-concepts (curies/query q2 'C))
    (for-each
     (lambda (c) (hash-update! counter c (lambda (x) (+ x 1)) 0))
     related-concepts)
    )
  (for-each step2 related-pubs)
  (sort-count counter)
  )
|#

;;(define covid-spike-genes (cluster-concepts-by-pubs spike-covid19 gene))
;;(define sars-spike-genes (related-count spike-sars gene))

;;(define covid-drugs (related-count covid19 drug))
;;(define sars-drugs (related-count sars drug))
;;(define mers-drugs (related-count mers drug))

(define (in-common c1 c2)

  (define length1 (length c1))
  (define length2 (length c2))
  
  (define (report-dups l1 l2)
    (if (or (null? l1) (null? l2)) '()
        (if (assoc (car (car l1)) l2)
            (cons (list (car (car l1)) (string-join (list (number->string (+ 1 (index-of c1 (car l1)))) (number->string length1)) ":") (string-join (list (number->string (+ 1 (index-of l2 (car l1)))) (number->string length2)) ":")) (report-dups (cdr l1) l2))
           (report-dups (cdr l1) l2))))

  (report-dups c1 c2)
  )

(define (differences c1 c2)

   (define (find-dups l1 l2)
    (if (or (null? l1) (null? l2)) '()
        (if (assoc (car (car l1)) l2)
            (cons (car (car l1)) (find-dups (cdr l1) l2))
           (find-dups (cdr l1) l2))))

   (define dups (find-dups c1 c2))

   (list (remove* dups (map car c1)) (remove* dups (map car c2)))
  )

;;(define test (related-count "UniProtKB:P0DTC2" #f))

;;(define results (remove-duplicates (map car (unwrap (map curie-synonyms/names (curies/query qcross 'O))))))
#|
(define edges (map (lambda (x)
                     (define qcross (query/graph
                                     ((S "UniProtKB:P0DTC2")
                                      (O x))
                                     ((S->O #f))
                                     (S S->O O)))

                     (edges/query qcross 'S->O)
                     )
                   all-synons))

(define q (query/graph
           ((S "UniProtKB:P0DTC2")
            (O #f))
           ((S->O #f))
           (S S->O O)))
|#

(define edges (make-hash))

;;CAN BE DONE MORE EFFICIENTLY
(define (interesting-es cluster curie)
  ;;~80s
  (define cluster-curies (time (remove-duplicates (map car (unwrap (map curie-synonyms/names (cdr (hash-keys cluster))))))))
  ;;~9s
  (define qcross (time (query/graph
                        ((S curie)
                         (O #f))
                        ((S->O #f))
                        (S S->O O))))
  ;;~7s
  (define curie-objects (time (remove-duplicates (map car (unwrap (map curie-synonyms/names (curies/query qcross 'O)))))))
  ;;~12s
  (define interesting-curies (time (find-dups cluster-curies curie-objects)))

  (for-each
   (lambda (c)
     ;;~4s each
     (define q2 (time (query/graph
                       ((S curie)
                        (O c))
                       ((S->O #f))
                       (S S->O O))))

     (define q3 (time (query/graph
                       ((S c)
                        (O curie))
                       ((S->O #f))
                       (S S->O O))))
     (define es (edges/query q2 'S->O))
     (for-each
      (lambda (e)
        (hash-update! edges (predicate e) (lambda (x) (append x (list c))) '()))
      es))
   (take interesting-curies 10))

  edges
  ;;(hash-for-each edges (lambda (k v) (hash-set! edges k (remove-duplicates v))))
  )




;;(define ace2-clust (related-count ace2 #f))
;;(define es (interesting-es ace2-clust ace2))
#|
(define (interesting-es cluster curie)
  (define cluster-curies (remove-duplicates (map car (unwrap (map curie-synonyms/names (map car cluster))))))

  (define edges (make-hash))

  (for-each
   (lambda (c)
     (define q2 (query/graph
                 ((S curie)
                  (O c))
                 ((S->O #f))
                 (S S->O O)))
     (for-each
      (lambda (e)
        (hash-update! edges (predicate e) (lambda (x) (append x (list c))) '()))
      (edges/query q2 'S->O)))
   cluster-curies)
  edges
  )
|#
#|
(define qtest (time (query/graph
               ((S ace2)
                (O #f))
               ((S->O #f))
               (S S->O O))))
|#
#|
(define qtest (time (query/graph
               ((S '("UMLS:C1422064"))
                (O "CUI:C1422064"))
               ((S->O #f))
               (S S->O O))))
|#

#|
(define qcross (query/graph
                ((S "UniProtKB:P0DTC2")
                 (O "CUI:C1826466"))
                ((S->O #f))
                (S S->O O)))
|#
;;(define gcounters (sort-counters (related-count spike-curie gene)))
;;(define pcounters (sort-counters (related-count spike-curie protein)))
;;(define dcounters (sort-counters (related-count spike-curie drug)))

#|
(define priority-counters
  (sort
   (hash-map counters (lambda (k v) (cons k v)))
   >
   #:key (lambda (x) (cdr x))))

(define partial (map (lambda (x)
       (cons (curie-synonyms/names (car x)) x))
     (take priority-counters 100)))
|#

#|
(define count-tester (make-hash))

#|
(define qt (query/graph
              ((C "UniProtKB:P0DTC2")
               (P #f))
              ((C->P  '("biolink:related_to")))
              (C C->P P)))
|#

(define qt2 (query/graph
                ((C #f)
                 (P "CORD:0001418189999fea7f7cbe3e82703d71c85a6fe5"))
                ((C->P  '("biolink:related_to")))
                (C C->P P)))

(define related-c-test (curies/query qt2 'C))

(define (step2 p)
    (printf ".\n")
    (define q2 (query/graph
                ((C #f)
                 (P p))
                ((C->P  '("biolink:related_to")))
                (C C->P P)))
    (define related-concepts (curies/query q2 'C))
    (for-each
     (lambda (c) (hash-update! count-tester c (lambda (x) (+ x 1)) 0))
     related-concepts))
#|
(for-each
     (lambda (c) (hash-update! count-tester c (lambda (x) (+ x 1)) 0))
     related-c-test)
|#
|#