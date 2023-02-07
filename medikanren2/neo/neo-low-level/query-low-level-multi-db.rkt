#lang racket/base

(provide
  query:Known->Known
  query:Known->X
  query:X->Known
  query:Known<-X->Known
  query:Known->X->Known
  query:X->Y->Known
  query:Concept
  concept-properties
  ;;concept-property-values
  curie-in-db?
  curie->properties
  edge-properties
  ;;edge-property-values
  ;;edge-id->properties
  curie->synonyms
  curies->synonyms
  )

(require

 (rename-in "query-low-level-robokop.rkt"
            (query:Known->Known query:Known->Known-robokop)
            (query:Known->X query:Known->X-robokop)
            (query:X->Known query:X->Known-robokop)
            (query:Known<-X->Known query:Known<-X->Known-robokop)
            (query:Known->X->Known query:Known->X->Known-robokop)
            (query:X->Y->Known query:X->Y->Known-robokop)
            (query:Concept query:Concept-robokop)
            (concept-properties concept-properties-robokop)
            (concept-property-values concept-property-values-robokop)
            (curie-in-db? curie-in-db?-robokop)
            (curie->properties curie->properties-robokop)
            (edge-properties edge-properties-robokop)
            (edge-property-values edge-property-values-robokop)
            (edge-id->properties edge-id->properties-robokop)
            )

 (rename-in "query-low-level-text-mining.rkt"
            (query:Known->Known query:Known->Known-text-mining)
            (query:Known->X query:Known->X-text-mining)
            (query:X->Known query:X->Known-text-mining)
            (query:Known<-X->Known query:Known<-X->Known-text-mining)
            (query:Known->X->Known query:Known->X->Known-text-mining)
            (query:X->Y->Known query:X->Y->Known-text-mining)
            (query:Concept query:Concept-text-mining)
            (concept-properties concept-properties-text-mining)
            (concept-property-values concept-property-values-text-mining)
            (curie-in-db? curie-in-db?-text-mining)
            (curie->properties curie->properties-text-mining)
            (edge-properties edge-properties-text-mining)
            (edge-property-values edge-property-values-text-mining)
            (edge-id->properties edge-id->properties-text-mining)
            )

 (rename-in "query-low-level-rtx-kg2.rkt"
            (query:Known->Known query:Known->Known-rtx-kg2)
            (query:Known->X query:Known->X-rtx-kg2)
            (query:X->Known query:X->Known-rtx-kg2)
            (query:Known<-X->Known query:Known<-X->Known-rtx-kg2)
            (query:Known->X->Known query:Known->X->Known-rtx-kg2)
            (query:X->Y->Known query:X->Y->Known-rtx-kg2)
            (query:Concept query:Concept-rtx-kg2)
            (concept-properties concept-properties-rtx-kg2)
            (concept-property-values concept-property-values-rtx-kg2)
            (curie-in-db? curie-in-db?-rtx-kg2)
            (curie->properties curie->properties-rtx-kg2)
            (edge-properties edge-properties-rtx-kg2)
            (edge-property-values edge-property-values-rtx-kg2)
            (edge-id->properties edge-id->properties-rtx-kg2)
            )
 
 (rename-in "query-low-level-node-normalization.rkt"
            (query:Known->X query:Known->X-node-normalization)
            (query:X->Known query:X->Known-node-normalization)
            (curie-in-db? curie-in-db?-node-normalization)
            )

 racket/set
 "../dbKanren/dbk/database.rkt")

#|
Given a single CURIE, returns a list of CURIEs containing synonyms for the
original CURIE.  The list of synonyms includes the original provided CURIE.
|#
(define (curie->synonyms curie) (curies->synonyms (list curie)))

#|
Given a list of CURIEs, returns a list of CURIEs containing synonyms for the
CURIEs in the original list.  The list of synonyms includes the original
provided list of CURIEs.
|#
(define (curies->synonyms curies)
  (set->list
   (set-union
    ; the original provided list of CURIEs
    (list->set curies)
    ; the synonyms from node normalization KG
    (set-fixed-point
     (list->set (filter curie-in-db?-node-normalization curies))
     (lambda (new-curies)
       (query:one-hop-same-as new-curies
                              query:X->Known-node-normalization
                              query:Known->X-node-normalization
                              )))
    ; the synonyms from other [rtx-kg2, text-mining, robokop] KGs
    (set-fixed-point
     (list->set curies)
     (lambda (new-curies)
       (query:one-hop-same-as new-curies
                              query:X->Known
                              query:Known->X
                              )))
    )))

(define query:one-hop-same-as
  (lambda (set-curies query:X->Known query:Known->X)
    (set-union
     (list->set
      (map car
           (query:X->Known
            #f
            (list "biolink:same_as")
            (set->list set-curies))))
     (list->set
      (map (lambda (e) (list-ref e 3))
           (query:Known->X
            (set->list set-curies)
            (list "biolink:same_as")
            #f))))))

(define (query:Known->Known curie*.S predicate*.S->O curie*.O)
  (set-union
   (query:Known->Known-robokop
    (filter curie-in-db?-robokop curie*.S)
    (filter curie-in-db?-robokop predicate*.S->O)
    (filter curie-in-db?-robokop curie*.O))
   (query:Known->Known-text-mining
    (filter curie-in-db?-text-mining curie*.S)
    (filter curie-in-db?-text-mining predicate*.S->O)
    (filter curie-in-db?-text-mining curie*.O))
   (query:Known->Known-rtx-kg2
    (filter curie-in-db?-rtx-kg2 curie*.S)
    (filter curie-in-db?-rtx-kg2 predicate*.S->O)
    (filter curie-in-db?-rtx-kg2 curie*.O))))

(define (query:Known->X curie*.K predicate*.K->X category*.X)
  (set-union
   (query:Known->X-robokop
    (filter curie-in-db?-robokop curie*.K)
    (filter curie-in-db?-robokop predicate*.K->X)
    (and category*.X
         (filter curie-in-db?-robokop category*.X)))
   (query:Known->X-text-mining
    (filter curie-in-db?-text-mining curie*.K)
    (filter curie-in-db?-text-mining predicate*.K->X)
    (and category*.X
         (filter curie-in-db?-text-mining category*.X)))
   (query:Known->X-rtx-kg2
    (filter curie-in-db?-rtx-kg2 curie*.K)
    (filter curie-in-db?-rtx-kg2 predicate*.K->X)
    (and category*.X
         (filter curie-in-db?-rtx-kg2 category*.X)))))

(define (query:X->Known category*.X predicate*.X->K curie*.K)
  (set-union
   (query:X->Known-robokop
    (and category*.X
         (filter curie-in-db?-robokop category*.X))
    (filter curie-in-db?-robokop predicate*.X->K)
    (filter curie-in-db?-robokop curie*.K))
   (query:X->Known-text-mining
    (and category*.X
         (filter curie-in-db?-text-mining category*.X))
    (filter curie-in-db?-text-mining predicate*.X->K)
    (filter curie-in-db?-text-mining curie*.K))
   (query:X->Known-rtx-kg2
    (and category*.X
         (filter curie-in-db?-rtx-kg2 category*.X))
    (filter curie-in-db?-rtx-kg2 predicate*.X->K)
    (filter curie-in-db?-rtx-kg2 curie*.K))))

(define (query:Known<-X->Known curie*.K1 predicate*.K1<-X category*.X predicate*.X->K2 curie*.K2)
  (set-union
   (query:Known<-X->Known-robokop
    (filter curie-in-db?-robokop curie*.K1)
    (filter curie-in-db?-robokop predicate*.K1<-X)
    (and category*.X
         (filter curie-in-db?-robokop category*.X))
    (filter curie-in-db?-robokop predicate*.X->K2)
    (filter curie-in-db?-robokop curie*.K2))
   (query:Known<-X->Known-text-mining
    (filter curie-in-db?-text-mining curie*.K1)
    (filter curie-in-db?-text-mining predicate*.K1<-X)
    (and category*.X
         (filter curie-in-db?-text-mining category*.X))
    (filter curie-in-db?-text-mining predicate*.X->K2)
    (filter curie-in-db?-text-mining curie*.K2))
   (query:Known<-X->Known-rtx-kg2
    (filter curie-in-db?-rtx-kg2 curie*.K1)
    (filter curie-in-db?-rtx-kg2 predicate*.K1<-X)
    (and category*.X
         (filter curie-in-db?-rtx-kg2 category*.X))
    (filter curie-in-db?-rtx-kg2 predicate*.X->K2)
    (filter curie-in-db?-rtx-kg2 curie*.K2))))

(define (query:Known->X->Known curie*.K1 predicate*.K1->X category*.X predicate*.X->K2 curie*.K2)
  (set-union
   (query:Known->X->Known-robokop
    (filter curie-in-db?-robokop curie*.K1)
    (filter curie-in-db?-robokop predicate*.K1->X)
    (and category*.X
         (filter curie-in-db?-robokop category*.X))
    (filter curie-in-db?-robokop predicate*.X->K2)
    (filter curie-in-db?-robokop curie*.K2))
   (query:Known->X->Known-text-mining
    (filter curie-in-db?-text-mining curie*.K1)
    (filter curie-in-db?-text-mining predicate*.K1->X)
    (and category*.X
         (filter curie-in-db?-text-mining category*.X))
    (filter curie-in-db?-text-mining predicate*.X->K2)
    (filter curie-in-db?-text-mining curie*.K2))
   (query:Known->X->Known-rtx-kg2
    (filter curie-in-db?-rtx-kg2 curie*.K1)
    (filter curie-in-db?-rtx-kg2 predicate*.K1->X)
    (and category*.X
         (filter curie-in-db?-rtx-kg2 category*.X))
    (filter curie-in-db?-rtx-kg2 predicate*.X->K2)
    (filter curie-in-db?-rtx-kg2 curie*.K2))))

(define (query:X->Y->Known category*.X predicate*.X->Y category*.Y predicate*.Y->K curie*.K)
  (set-union
   (query:X->Y->Known-robokop
    (and category*.X
         (filter curie-in-db?-robokop category*.X))
    (filter curie-in-db?-robokop predicate*.X->Y)
    (and category*.Y
         (filter curie-in-db?-robokop category*.Y))
    (filter curie-in-db?-robokop predicate*.Y->K)
    (filter curie-in-db?-robokop curie*.K))
   (query:X->Y->Known-text-mining
    (and category*.X
         (filter curie-in-db?-text-mining category*.X))
    (filter curie-in-db?-text-mining predicate*.X->Y)
    (and category*.Y
         (filter curie-in-db?-text-mining category*.Y))
    (filter curie-in-db?-text-mining predicate*.Y->K)
    (filter curie-in-db?-text-mining curie*.K))
   (query:X->Y->Known-rtx-kg2
    (and category*.X
         (filter curie-in-db?-rtx-kg2 category*.X))
    (filter curie-in-db?-rtx-kg2 predicate*.X->Y)
    (and category*.Y
         (filter curie-in-db?-rtx-kg2 category*.Y))
    (filter curie-in-db?-rtx-kg2 predicate*.Y->K)
    (filter curie-in-db?-rtx-kg2 curie*.K))))

(define (query:Concept curie*)
  (set-union
   (query:Concept-robokop (filter curie-in-db?-robokop curie*))
   (query:Concept-text-mining (filter curie-in-db?-text-mining curie*))
   (query:Concept-rtx-kg2 (filter curie-in-db?-rtx-kg2 curie*))))

(define (concept-properties)
  (set-union
   (concept-properties-robokop)
   (concept-properties-text-mining)
   (concept-properties-rtx-kg2)))

;;(define (concept-property-values key) 'TODO)

(define (curie-in-db? curie)
  (or
   (curie-in-db?-robokop curie)
   (curie-in-db?-text-mining curie)
   (curie-in-db?-rtx-kg2 curie)))

(define (curie->properties curie)
  (set-union
   (if (curie-in-db?-robokop curie)
       (curie->properties-robokop curie)
       '())
   (if (curie-in-db?-text-mining curie)
       (curie->properties-text-mining curie)
       '())
   (if (curie-in-db?-rtx-kg2 curie)
       (curie->properties-rtx-kg2 curie)
       '())))

(define (edge-properties)
  (set-union
   (edge-properties-robokop)
   (edge-properties-text-mining)
   (edge-properties-rtx-kg2)))

;;(define (edge-property-values key) 'TODO)
;;(define (edge-id->properties eid) 'TODO)
