#lang racket/base

(provide
 query:Known->Known
 query:Known->X
 query:X->Known
 query:Known<-X->Known
 query:Known->X->Known
 query:X->Y->Known
 query:Known->Y->X
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

  #;(rename-in "query-low-level-rtx-kg2.rkt"
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
  "query-low-level-equivalence.rkt"
  "../neo-utils/neo-helpers-without-db.rkt"
  "../dbKanren/dbk/database.rkt"
  "../dbKanren/dbk/enumerator.rkt"
  "../dbKanren/dbk/stream.rkt"
  ;;racket/fixnum
  racket/match
  racket/set
  racket/list
  )

(define (query:Known->Known curie*.S predicate*.S->O curie*.O)
  (append
   (query:Known->Known-robokop
    (filter curie-in-db?-robokop curie*.S)
    (filter curie-in-db?-robokop predicate*.S->O)
    (filter curie-in-db?-robokop curie*.O))
   (query:Known->Known-text-mining
    (filter curie-in-db?-text-mining curie*.S)
    (filter curie-in-db?-text-mining predicate*.S->O)
    (filter curie-in-db?-text-mining curie*.O))
   #;(query:Known->Known-rtx-kg2
    (filter curie-in-db?-rtx-kg2 curie*.S)
    (filter curie-in-db?-rtx-kg2 predicate*.S->O)
    (filter curie-in-db?-rtx-kg2 curie*.O))))

(define (query:Known->X curie*.K predicate*.K->X category*.X)
  (append
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
   #;(query:Known->X-rtx-kg2
    (filter curie-in-db?-rtx-kg2 curie*.K)
    (filter curie-in-db?-rtx-kg2 predicate*.K->X)
    (and category*.X
         (filter curie-in-db?-rtx-kg2 category*.X)))))

(define (query:X->Known category*.X predicate*.X->K curie*.K)
  (append
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
   #;(query:X->Known-rtx-kg2
    (and category*.X
         (filter curie-in-db?-rtx-kg2 category*.X))
    (filter curie-in-db?-rtx-kg2 predicate*.X->K)
    (filter curie-in-db?-rtx-kg2 curie*.K))))

;; query:Known<-X->Known is analogous to a miniKanren-style query with this shape:
;;(run* (K1 name.K1 predicates.K1<-X X name.X predicates.X->K1 K2 name.K2)
;;  (fresh (id1 id2 category.X)
;;    (edge id1 X K1)
;;    (edge id2 X K2)
;;    (cprop X   "category" category.X)
;;    (cprop X   "name" name.X)
;;    (cprop K1  "name" name.K1)
;;    (cprop K2  "name" name.K2)
;;    (eprop id1 "predicate" K1<-X)
;;    (eprop id2 "predicate" X->K2)
;;    (membero category.X categories.X)
;;    (membero K1         curies.K1)
;;    (membero K1<-X      predicates.K1<-X)
;;    (membero K2         curies.K2)
;;    (membero X->K2      predicates.X->K2)))

;; Question:  Are we missing potential answers, either between KGs or within a single KG,
;; due to 'X' using more than one CURIE (HGNC vs. ENSMBL, for example)?
(define (query:Known<-X->Known curie*.K1 predicate*.K1<-X category*.X predicate*.X->K2 curie*.K2)
  (query:Known<-X->Known-helper
   (filter curie-in-db? curie*.K1)
   (filter curie-in-db? predicate*.K1<-X)
   (and category*.X
        (filter curie-in-db? category*.X))
   (filter curie-in-db? predicate*.X->K2)
   (filter curie-in-db? curie*.K2)))

(define (query:Known<-X->Known-helper curie*.K1 predicate*.K1<-X category*.X predicate*.X->K2 curie*.K2)
  (define (candidate*->dict candidate*)
    (let* ((candidate* (sort candidate* (lambda (a b) (string<? (car a) (car b)))))
           (group*     (list->vector (s-group candidate* equal? car)))
           (ref.value  (lambda (i) (vector-ref group* i))))
      (dict:ref (lambda (i) (caar (ref.value i))) string<?
                ref.value 0 (vector-length group*))))
  (let* ((X=>XK1=>1 (candidate*->dict (query:X->Known category*.X predicate*.K1<-X curie*.K1)))
         (X=>XK2=>1 (candidate*->dict (query:X->Known category*.X predicate*.X->K2 curie*.K2))))
    (maybe-time (enumerator->list
                 (lambda (yield)
                   ((merge-join string<? X=>XK1=>1 X=>XK2=>1)
                    (lambda (X XK1* XK2*)
                      (for-each
                       (lambda (XK1)
                         (match-define (list* _ predicate.X->K1 K1 props1) XK1)
                         (for-each
                          (lambda (XK2)
                            (match-define (list* _ X->K2 K2 props2) XK2)
                            (yield (append (list K1 predicate.X->K1 X X->K2 K2)
                                           (append props1 props2))))
                          XK2*))
                       XK1*))))))))


(define (query:Known->X->Known curie*.K1 predicate*.K1->X category*.X predicate*.X->K2 curie*.K2)
  (query:Known->X->Known-helper
   (filter curie-in-db? curie*.K1)
   (filter curie-in-db? predicate*.K1->X)
   (and category*.X
        (filter curie-in-db? category*.X))
   (filter curie-in-db? predicate*.X->K2)
   (filter curie-in-db? curie*.K2)))

(define (query:Known->X->Known-helper curie*.K1 predicate*.K1->X category*.X predicate*.X->K2 curie*.K2)
  (define (KX*->dict candidate*)
    (let* ((candidate* (sort candidate* (lambda (a b) (string<? (caddr a) (caddr b)))))
           (group*     (list->vector (s-group candidate* equal? caddr)))
           (ref.value  (lambda (i) (vector-ref group* i))))
      (dict:ref (lambda (i) (caddr (car (ref.value i)))) string<?
                ref.value 0 (vector-length group*))))
  (define (XK*->dict candidate*)
    (let* ((candidate* (sort candidate* (lambda (a b) (string<? (car a) (car b)))))
           (group*     (list->vector (s-group candidate* equal? car)))
           (ref.value  (lambda (i) (vector-ref group* i))))
      (dict:ref (lambda (i) (caar (ref.value i))) string<?
                ref.value 0 (vector-length group*))))
  (let* ((X=>K1X* (KX*->dict (query:Known->X curie*.K1 predicate*.K1->X category*.X)))
         (X=>XK2* (XK*->dict (query:X->Known category*.X predicate*.X->K2 curie*.K2))))
    (maybe-time (enumerator->list
                 (lambda (yield)
                   ((merge-join string<? X=>K1X* X=>XK2*)
                    (lambda (X K1X* XK2*)
                      (for-each
                       (lambda (K1X)
                         (match-define (list* K1 predicate.X->K1 _ props1) K1X)
                         (for-each
                          (lambda (XK2)
                            (match-define (list* _ X->K2 K2 props2) XK2)
                            (yield (append (list K1 predicate.X->K1 X X->K2 K2)
                                           (append props1 props2))))
                          XK2*))
                       K1X*))))))))

(define (result*->dict key result* curie-rep-hash)
    (let* ((get-representative (lambda (tuple) (hash-ref curie-rep-hash (key tuple))))
           (rep/result* (map (lambda (tuple) (cons (get-representative tuple) tuple)) result*))
           (rep/key car)
           (rep/result*  (sort rep/result* (lambda (a b) (string<? (rep/key a) (rep/key b)))))
           (group*    (list->vector (s-group rep/result* equal? rep/key)))
           (ref.value (lambda (i) (vector-ref group* i))))
      (dict:ref (lambda (i) (rep/key (car (ref.value i)))) string<?
                ref.value 0 (vector-length group*))))

(define find-smallest-string
  (lambda (string*)
    (let loop ((s* (cdr string*)) (smallest (car string*)))
      (cond
        ((null? s*) smallest)
        (else
         (if (string<? (car s*) smallest)
             (loop (cdr s*) (car s*))
             (loop (cdr s*) smallest)))))))

(define build-curie-representative-hash
  (lambda (curie*)
    (define build-curie-representative-hash
      (lambda (hash curie)
        (if (hash-has-key? hash curie)
            hash
            (let* ((synonyms (curie->synonyms curie))
                   (representative (find-smallest-string synonyms)))
              (let loop ((h hash) (s* synonyms))
                (cond
                  ((null? s*) h)
                  (else (loop (hash-set h (car s*) representative) (cdr s*)))))))))
    (let loop ((h (hash)) (c* curie*))
      (cond
        ((null? c*) h)
        (else (loop (build-curie-representative-hash h (car c*)) (cdr c*)))))))

(define (query:X->Y->Known category*.X predicate*.X->Y category*.Y predicate*.Y->K curie*.K)
  (query:X->Y->Known-helper
   (and category*.X
        (filter curie-in-db? category*.X))
   (filter curie-in-db? predicate*.X->Y)
   (and category*.Y
        (filter curie-in-db? category*.Y))
   (filter curie-in-db? predicate*.Y->K)
   (filter curie-in-db? curie*.K)))

(define (query:X->Y->Known-helper category*.X predicate*.X->Y category*.Y predicate*.Y->K curie*.K)  
  (let* ((YK (query:X->Known category*.Y predicate*.Y->K curie*.K))
         (curie-rep-hash (build-curie-representative-hash (remove-duplicates (map car YK))))
         (Y=>YK=>1 (result*->dict car YK curie-rep-hash))
         (curie*.Y (hash-keys curie-rep-hash))
         (XY (query:X->Known category*.X predicate*.X->Y curie*.Y))
         (Y=>XY=>1 (result*->dict caddr XY curie-rep-hash)))
    (maybe-time (enumerator->list
                 (lambda (yield)
                   ((merge-join string<? Y=>XY=>1 Y=>YK=>1)
                    (lambda (rep XY* YK*)
                      (for-each
                       (lambda (XY)
                         (match-define (list* _ X predicate.X->Y Y props.X->Y) XY)
                         (for-each
                          (lambda (YK)
                            (match-define (list* _ _ Y->K K props.Y->K) YK)
                            (yield (list X predicate.X->Y Y Y->K K props.X->Y props.Y->K)))
                          YK*))
                       XY*))))))))

(define (query:Known->Y->X curie*.K predicate*.K->Y category*.Y predicate*.Y->X category*.X)
  (query:Known->Y->X-helper
   (filter curie-in-db? curie*.K)
   (filter curie-in-db? predicate*.K->Y)
   (and category*.Y
        (filter curie-in-db? category*.Y))
   (filter curie-in-db? predicate*.Y->X)
   (and category*.X
        (filter curie-in-db? category*.X))))

(define (query:Known->Y->X-helper curie*.K predicate*.K->Y category*.Y predicate*.Y->X category*.X )  
  (let* ((KY (query:Known->X curie*.K predicate*.K->Y category*.Y))
         (curie-rep-hash (build-curie-representative-hash (remove-duplicates (map caddr KY))))
         (Y=>KY=>1 (result*->dict caddr KY curie-rep-hash))
         (curie*.Y (hash-keys curie-rep-hash))
         (YX (query:Known->X curie*.Y predicate*.Y->X category*.X))
         (Y=>YX=>1 (result*->dict car YX curie-rep-hash)))
    (maybe-time (enumerator->list
                 (lambda (yield)
                   ((merge-join string<? Y=>YX=>1 Y=>KY=>1)
                    (lambda (rep YX* KY*)
                      (for-each
                       (lambda (YX)
                         (match-define (list* _ Y predicate.Y->X X props.Y->X) YX)
                         (for-each
                          (lambda (KY)
                            (match-define (list* _ K K->Y _ props.K->Y) KY)
                            (yield (list K K->Y Y predicate.Y->X X props.K->Y props.Y->X)))
                          KY*))
                       YX*))))))))

(define (query:Concept curie*)
  (append
   (query:Concept-robokop (filter curie-in-db?-robokop curie*))
   (query:Concept-text-mining (filter curie-in-db?-text-mining curie*))
   #;(query:Concept-rtx-kg2 (filter curie-in-db?-rtx-kg2 curie*))))

(define (concept-properties)
  (append
   (concept-properties-robokop)
   (concept-properties-text-mining)
   #;(concept-properties-rtx-kg2)))

;;(define (concept-property-values key) 'TODO)

(define (curie-in-db? curie)
  (or
   (curie-in-db?-robokop curie)
   (curie-in-db?-text-mining curie)
   #;(curie-in-db?-rtx-kg2 curie)))

(define (curie->properties curie)
  (append
   (if (curie-in-db?-robokop curie)
       (curie->properties-robokop curie)
       '())
   (if (curie-in-db?-text-mining curie)
       (curie->properties-text-mining curie)
       '())
   #;(if (curie-in-db?-rtx-kg2 curie)
       (curie->properties-rtx-kg2 curie)
       '())))

(define (edge-properties)
  (append
   (edge-properties-robokop)
   (edge-properties-text-mining)
   #;(edge-properties-rtx-kg2)))

;;(define (edge-property-values key) 'TODO)
;;(define (edge-id->properties eid) 'TODO)
