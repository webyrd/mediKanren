#lang racket/base

;; adapted from
;;
;; https://github.com/gregr/dbKanren/blob/master/test/test-low-level.rkt

(provide
 make-query-low-level)
(require
  "../dbKanren/dbk/database.rkt"
  "../dbKanren/dbk/enumerator.rkt"
  "../dbKanren/dbk/logging.rkt"
  "../dbKanren/dbk/stream.rkt"
  "../neo-utils/neo-helpers-without-db.rkt"
  racket/fixnum racket/match racket/pretty racket/runtime-path racket/set racket/list)

(define-runtime-path path.here "../neo-data")

(define NAME_NOT_FOUND_STRING "N/A")


(define (make-query-low-level
         db-path-under-parent ;; for example, "rtx-kg2/pre_2.8.0/rtx-kg2pre_2.8.0.db"
         )

  (pretty-log `(In make-query-low-level for)
              db-path-under-parent)
  
  (define str.predicate "predicate")
  ;;(define str.predicate "edge_label")

  (define (curie-in-db? curie)
    (initialize-text!)
    (dict-ref (thread-cell-ref tcell.text=>id)
              (string->bytes/utf-8 curie)
              (lambda (id) #t)
              (lambda () #f)))

  (define (curies-in-db curie*)
    (initialize-text!)
    (define curie-text*=>1
      (let* ((text* (map string->bytes/utf-8 curie*))
             (text* (list->vector (sort (remove-duplicates text*) bytes<?))))
        (dict:ref (lambda (i) (vector-ref text* i)) bytes<?
                  (lambda (_) '()) 0 (vector-length text*))))
    (define (helper yield)
      ((merge-join bytes<? curie-text*=>1 (thread-cell-ref tcell.text=>id))
       (lambda (text.curie __ ___)
         (yield (bytes->string/utf-8 text.curie)))))
    (enumerator->rlist helper))

  (define (dict-get d key)
    (dict-ref d key (lambda (v) v) (lambda () (error "dict-get failed" key))))

  (define (dict-get-safe d key not-found-value)
    (dict-ref d key (lambda (v) v) (lambda () not-found-value)))

  (define (get-name-from-dict-safe d k)
    (if d
        (let* ((name? (dict-get-safe d k #f))
               (name (if name? (dict-min name?) #f)))
          (if name (id->string name) NAME_NOT_FOUND_STRING))
        NAME_NOT_FOUND_STRING))
  
  (define (string*->id=>1 str*) (bytes*->id=>1 (map string->bytes/utf-8 str*)))
  (define (bytes*->id=>1 text*)
    (let* ((text* (sort (set->list (list->set text*)) bytes<?))
           (id*   (list->vector (map text->id text*))))
      (dict:ref (lambda (i) (vector-ref id* i)) fx<
                (lambda (_) '()) 0 (vector-length id*))))

  (define (int*->id=>1 int*)
    (let ((id* (list->vector (sort (remove-duplicates int*) fx<))))
      (dict:ref (lambda (i) (vector-ref id* i)) fx<
                (lambda (_) '()) 0 (vector-length id*))))

  (define (string->id str) (text->id (string->bytes/utf-8 str)))
  (define (id->string id)  (bytes->string/utf-8 (id->text id)))
  (define (text->id   b)   (initialize-text!) (dict-get (thread-cell-ref tcell.text=>id) b))
  (define (id->text   id)  (initialize-text!) (dict-get (thread-cell-ref tcell.id=>text) id))

  (define (concept-properties)          (map id->string (enumerator->list
                                                         (dict-key-enumerator ckey=>cvalue=>curie=>1))))
  (define (edge-properties)             (map id->string (enumerator->list
                                                         (dict-key-enumerator ekey=>evalue=>eid=>1))))
  (define (concept-property-values key) (s-map id->string (enumerator->s
                                                           (dict-key-enumerator
                                                            (dict-get ckey=>cvalue=>curie=>1
                                                                      (string->id key))))))
  (define (edge-property-values    key) (s-map id->string (enumerator->s
                                                           (dict-key-enumerator
                                                            (dict-get ekey=>evalue=>eid=>1
                                                                      (string->id key))))))
  (define (curie->properties curie)
    (enumerator->list
     (lambda (yield)
       ((dict-enumerator (dict-get curie=>ckey=>cvalue=>1 (string->id curie)))
        (lambda (ckey cvalue=>1)
          (yield (map id->string (cons ckey (enumerator->list
                                             (dict-key-enumerator cvalue=>1))))))))))
  (define (edge-id->properties eid)
    (enumerator->list
     (lambda (yield)
       ((dict-enumerator (dict-get eid=>ekey=>evalue=>1 eid))
        (lambda (ekey evalue=>1)
          (yield (map id->string (cons ekey (enumerator->list
                                             (dict-key-enumerator evalue=>1))))))))))

  (define (query:Known->X curie*.K predicate*.K->X category*.X)
    (define (query. yield)
      (let* ((ekey.predicate (string->id str.predicate))
             (K=>1           (string*->id=>1 curie*.K)))
        ((merge-join fx< K=>1 subject=>object=>eid=>1)
         (lambda (id.K __ X=>eid=>1)
           (let* ((K      (id->string id.K)))
             ((dict-enumerator X=>eid=>1)
              (lambda (id.X eid=>1)
                (let* ((X      (id->string id.X)))
                  ((dict-key-enumerator eid=>1)
                   (lambda (eid)
                     (let ((predicate.K->X
                            (id->string (dict-min (dict-get (dict-get eid=>ekey=>evalue=>1 eid)
                                                            ekey.predicate)))))
                       (yield (list* K predicate.K->X X (edge-id->properties eid))))))))))))))
    (define (query.c yield)
      (let* ((ekey.predicate     (string->id str.predicate))
             (ckey.category      (string->id "category"))
             (K=>1               (string*->id=>1 curie*.K))
             (category=>1        (string*->id=>1 category*.X))
             (category=>curie=>1 (dict-get ckey=>cvalue=>curie=>1 ckey.category)))
        ((merge-join fx< K=>1 subject=>object=>eid=>1)
         (lambda (id.K __ X=>eid=>1)
           (let* ((K      (id->string id.K)))
             ((merge-join fx< category=>1 category=>curie=>1)
              (lambda (__ ___ X=>1.cprop)
                ((merge-join fx< X=>eid=>1 X=>1.cprop)
                 (lambda (id.X eid=>1 __)
                   (let* ((X      (id->string id.X)))
                     ((dict-key-enumerator eid=>1)
                      (lambda (eid)
                        (let ((predicate.K->X
                               (id->string (dict-min (dict-get (dict-get eid=>ekey=>evalue=>1 eid)
                                                               ekey.predicate)))))
                          (yield (list* K predicate.K->X X (edge-id->properties eid))))))))))))))))
    (define (query.p yield)
      (let* ((ekey.predicate     (string->id str.predicate))
             (K=>1               (string*->id=>1 curie*.K))
             (predicate=>1       (string*->id=>1 predicate*.K->X))
             (predicate=>eid=>1  (dict-get ekey=>evalue=>eid=>1 ekey.predicate)))
        ((merge-join fx< K=>1 subject=>eid=>object=>1)
         (lambda (id.K __ eid=>X=>1)
           (let* ((K         (id->string id.K)))
             ((merge-join fx< predicate=>1 predicate=>eid=>1)
              (lambda (id.predicate.K->X __ eid=>1)
                (let ((predicate.K->X (id->string id.predicate.K->X)))
                  ((merge-join fx< eid=>1 eid=>X=>1)
                   (lambda (eid __ X=>1.edge)
                     ((dict-key-enumerator X=>1.edge)
                      (lambda (id.X)
                        (let* ((X      (id->string id.X)))
                          (yield (list* K predicate.K->X X (edge-id->properties eid))))))))))))))))
    (define (query.p&c yield)
      (let* ((ekey.predicate     (string->id str.predicate))
             (ckey.category      (string->id "category"))
             (K=>1               (string*->id=>1 curie*.K))
             (predicate=>1       (string*->id=>1 predicate*.K->X))
             (category=>1        (string*->id=>1 category*.X))
             (predicate=>eid=>1  (dict-get ekey=>evalue=>eid=>1   ekey.predicate))
             (category=>curie=>1 (dict-get ckey=>cvalue=>curie=>1 ckey.category)))
        ((merge-join fx< K=>1 subject=>eid=>object=>1)
         (lambda (id.K __ eid=>X=>1)
           (let* ((K         (id->string id.K)))
             ((merge-join fx< predicate=>1 predicate=>eid=>1)
              (lambda (id.predicate.K->X __ eid=>1)
                (let ((predicate.K->X (id->string id.predicate.K->X)))
                  ((merge-join fx< eid=>1 eid=>X=>1)
                   (lambda (eid __ X=>1.edge)
                     ((merge-join fx< category=>1 category=>curie=>1)
                      (lambda (__ ___ X=>1.cprop)
                        ((merge-join fx< X=>1.cprop X=>1.edge)
                         (lambda (id.X __ ___)
                           (let* ((X      (id->string id.X)))
                             (yield (list* K predicate.K->X X (edge-id->properties eid))))))))))))))))))
    (maybe-time (enumerator->rlist (if predicate*.K->X
                                       (if category*.X query.p&c query.p)
                                       (if category*.X query.c   query.)))))

  (define (query:Known->X-scored curie*.K predicate*.K->X category*.X score*)

    (define (query.p yield)
      (let* ((K=>1               (string*->id=>1 curie*.K))
             (predicate=>1       (string*->id=>1 predicate*.K->X))
             (score=>1           (int*->id=>1 score*)))
        ((merge-join fx< score=>1 score=>pred=>sub=>obj=>eid=>1)
         (lambda (__ ___ pred=>sub=>obj=>eid=>1)
           ((merge-join fx< predicate=>1 pred=>sub=>obj=>eid=>1)
            (lambda (id.predicate.K->X __ sub=>obj=>eid=>1)
              (let ((predicate.K->X (id->string id.predicate.K->X)))
                ((merge-join fx< K=>1 sub=>obj=>eid=>1)
                 (lambda (id.K __ obj=>eid=>1)
                   (let ((K         (id->string id.K)))
                     ((dict-enumerator obj=>eid=>1)
                      (lambda (id.X eid=>1)
                        (let ((X      (id->string id.X)))
                          ((dict-key-enumerator eid=>1)
                              (lambda (eid)
                                (yield (list* K predicate.K->X X (edge-id->properties eid))))))))))))))))))
    (define (query.p&c yield)
      (let* ((ckey.category      (string->id "category"))
             (K=>1               (string*->id=>1 curie*.K))
             (predicate=>1       (string*->id=>1 predicate*.K->X))
             (score=>1           (int*->id=>1 score*))
             (category=>1        (string*->id=>1 category*.X))
             (category=>curie=>1 (dict-get ckey=>cvalue=>curie=>1 ckey.category)))
        ((merge-join fx< score=>1 score=>pred=>sub=>obj=>eid=>1)
         (lambda (__ ___ pred=>sub=>obj=>eid=>1)
           ((merge-join fx< predicate=>1 pred=>sub=>obj=>eid=>1)
            (lambda (id.predicate.K->X __ sub=>obj=>eid=>1)
              (let* ((predicate.K->X (id->string id.predicate.K->X)))
                ((merge-join fx< K=>1 sub=>obj=>eid=>1)
                    (lambda (id.K __ obj=>eid=>1)
                      (let* ((K         (id->string id.K)))
                        ((merge-join fx< category=>1 category=>curie=>1)
                         (lambda (__ ___ X=>1.cprop)
                           ((merge-join fx< X=>1.cprop obj=>eid=>1)
                            (lambda (id.X __ eid=>1)
                              (let* ((X      (id->string id.X)))
                                ((dict-key-enumerator eid=>1)
                                 (lambda (eid)
                                   (yield (list* K predicate.K->X X (edge-id->properties eid))))))))))))))))))))
    (if score*
        (if category*.X
            (maybe-time (enumerator->rlist query.p&c))
            (maybe-time (enumerator->rlist query.p)))
        '()))

  (define (query:X->Known category*.X predicate*.X->K curie*.K)
    (define (query. yield)
      (let* ((ekey.predicate (string->id str.predicate))
             (K=>1           (string*->id=>1 curie*.K)))
        ((merge-join fx< K=>1 object=>subject=>eid=>1)
         (lambda (id.K __ X=>eid=>1)
           (let* ((K      (id->string id.K)))
             ((dict-enumerator X=>eid=>1)
              (lambda (id.X eid=>1)
                (let* ((X      (id->string id.X)))
                  ((dict-key-enumerator eid=>1)
                   (lambda (eid)
                     (let ((predicate.X->K
                            (id->string (dict-min (dict-get (dict-get eid=>ekey=>evalue=>1 eid)
                                                            ekey.predicate)))))
                       (yield (list* X predicate.X->K K (edge-id->properties eid))))))))))))))
    (define (query.c yield)
      (let* ((ekey.predicate     (string->id str.predicate))
             (ckey.category      (string->id "category"))
             (K=>1               (string*->id=>1 curie*.K))
             (category=>1        (string*->id=>1 category*.X))
             (category=>curie=>1 (dict-get ckey=>cvalue=>curie=>1 ckey.category)))
        ((merge-join fx< K=>1 object=>subject=>eid=>1)
         (lambda (id.K __ X=>eid=>1)
           (let* ((K      (id->string id.K)))
             ((merge-join fx< category=>1 category=>curie=>1)
              (lambda (__ ___ X=>1.cprop)
                ((merge-join fx< X=>eid=>1 X=>1.cprop)
                 (lambda (id.X eid=>1 __)
                   (let* ((X      (id->string id.X)))
                     ((dict-key-enumerator eid=>1)
                      (lambda (eid)
                        (let ((predicate.X->K
                               (id->string (dict-min (dict-get (dict-get eid=>ekey=>evalue=>1 eid)
                                                               ekey.predicate)))))
                          (yield (list* X predicate.X->K K (edge-id->properties eid))))))))))))))))
    (define (query.p yield)
      (let* ((ekey.predicate     (string->id str.predicate))
             (K=>1               (string*->id=>1 curie*.K))
             (predicate=>1       (string*->id=>1 predicate*.X->K))
             (predicate=>eid=>1  (dict-get ekey=>evalue=>eid=>1   ekey.predicate)))
        ((merge-join fx< K=>1 object=>eid=>subject=>1)
         (lambda (id.K __ eid=>X=>1)
           (let* ((K      (id->string id.K)))
             ((merge-join fx< predicate=>1 predicate=>eid=>1)
              (lambda (id.predicate.X->K __ eid=>1)
                (let ((predicate.X->K (id->string id.predicate.X->K)))
                  ((merge-join fx< eid=>1 eid=>X=>1)
                   (lambda (eid __ X=>1.edge)
                     ((dict-key-enumerator X=>1.edge)
                      (lambda (id.X)
                        (let* ((X      (id->string id.X)))
                          (yield (list* X predicate.X->K K (edge-id->properties eid))))))))))))))))
    (define (query.p&c yield)
      (let* ((ekey.predicate     (string->id str.predicate))
             (ckey.category      (string->id "category"))
             (K=>1               (string*->id=>1 curie*.K))
             (predicate=>1       (string*->id=>1 predicate*.X->K))
             (category=>1        (string*->id=>1 category*.X))
             (predicate=>eid=>1  (dict-get ekey=>evalue=>eid=>1   ekey.predicate))
             (category=>curie=>1 (dict-get ckey=>cvalue=>curie=>1 ckey.category)))
        ((merge-join fx< K=>1 object=>eid=>subject=>1)
         (lambda (id.K __ eid=>X=>1)
           (let* ((K      (id->string id.K)))
             ((merge-join fx< predicate=>1 predicate=>eid=>1)
              (lambda (id.predicate.X->K __ eid=>1)
                (let ((predicate.X->K (id->string id.predicate.X->K)))
                  ((merge-join fx< eid=>1 eid=>X=>1)
                   (lambda (eid __ X=>1.edge)
                     ((merge-join fx< category=>1 category=>curie=>1)
                      (lambda (__ ___ X=>1.cprop)
                        ((merge-join fx< X=>1.cprop X=>1.edge)
                         (lambda (id.X __ ___)
                           (let* ((X      (id->string id.X)))
                             (yield (list* X predicate.X->K K (edge-id->properties eid))))))))))))))))))
    (maybe-time (enumerator->rlist (if predicate*.X->K
                                       (if category*.X query.p&c query.p)
                                       (if category*.X query.c   query.)))))

  (define (query:X->Known-scored category*.X predicate*.X->K curie*.K score*)
    (define (query.p yield)
      (let* ((K=>1               (string*->id=>1 curie*.K))
             (predicate=>1       (string*->id=>1 predicate*.X->K))
             (score=>1           (int*->id=>1 score*)))
        ((merge-join fx< score=>1 score=>pred=>obj=>sub=>eid=>1)
         (lambda (__ ___ pred=>obj=>sub=>eid=>1)
           ((merge-join fx< predicate=>1 pred=>obj=>sub=>eid=>1)
            (lambda (id.predicate.X->K __ obj=>sub=>eid=>1)
              (let ((predicate.X->K (id->string id.predicate.X->K)))
                ((merge-join fx< K=>1 obj=>sub=>eid=>1)
                 (lambda (id.K __ sub=>eid=>1)
                   (let ((K         (id->string id.K)))
                     ((dict-enumerator sub=>eid=>1)
                      (lambda (id.X eid=>1)
                        (let ((X      (id->string id.X)))
                          ((dict-key-enumerator eid=>1)
                              (lambda (eid)
                                (yield (list* X predicate.X->K K (edge-id->properties eid))))))))))))))))))
    (define (query.p&c yield)
      (let* ((ckey.category      (string->id "category"))
             (K=>1               (string*->id=>1 curie*.K))
             (predicate=>1       (string*->id=>1 predicate*.X->K))
             (score=>1           (int*->id=>1 score*))
             (category=>1        (string*->id=>1 category*.X))
             (category=>curie=>1 (dict-get ckey=>cvalue=>curie=>1 ckey.category)))
        ((merge-join fx< score=>1 score=>pred=>obj=>sub=>eid=>1)
         (lambda (__ ___ pred=>obj=>sub=>eid=>1)
           ((merge-join fx< predicate=>1 pred=>obj=>sub=>eid=>1)
            (lambda (id.predicate.X->K __ obj=>sub=>eid=>1)
              (let ((predicate.X->K (id->string id.predicate.X->K)))
                ((merge-join fx< K=>1 obj=>sub=>eid=>1)
                 (lambda (id.K __ sub=>eid=>1)
                   (let ((K         (id->string id.K)))
                     ((merge-join fx< category=>1 category=>curie=>1)
                      (lambda (__ ___ X=>1.cprop)
                        ((merge-join fx< X=>1.cprop sub=>eid=>1)
                         (lambda (id.X __ eid=>1)
                           (let ((X      (id->string id.X)))
                             ((dict-key-enumerator eid=>1)
                              (lambda (eid)
                                (yield (list* X predicate.X->K K (edge-id->properties eid))))))))))))))))))))
    (if score*
        (if category*.X
            (maybe-time (enumerator->rlist query.p&c))
            (maybe-time (enumerator->rlist query.p)))
        '()))


  (define (query:Known<-X->Known curie*.K1 predicate*.K1<-X category*.X predicate*.X->K2 curie*.K2)
    (error 'query:Known<-X->Known "obsolete"))

  (define (query:Known->X->Known curie*.K1 predicate*.K1->X category*.X predicate*.X->K2 curie*.K2)
    (error 'query:Known->X->Known "obsolete"))

  (define (query:X->Y->Known category*.X predicate*.X->Y category*.Y predicate*.Y->K curie*.K)
    (error 'query:X->Y->Known "obsolete"))

  (define (query:Known->Known curie*.S predicate*.S->O curie*.O)
    (if (> (length curie*.S) (length curie*.O))
        (query:dict.Known->dict.Known
         (string*->id=>1 curie*.S) predicate*.S->O (string*->id=>1 curie*.O) 'obj->sub)
        (query:dict.Known->dict.Known
         (string*->id=>1 curie*.S) predicate*.S->O (string*->id=>1 curie*.O) 'sub->obj)))

  (define (query:dict.Known->dict.Known curie=>1.S predicate*.S->O curie=>1.O direction-tag)
    (define (query.sub->obj yield)
      (let* ((ekey.predicate    (string->id str.predicate))
             (ckey.name         (string->id "name"))
             (predicate=>1      (string*->id=>1 predicate*.S->O))
             (predicate=>eid=>1 (dict-get ekey=>evalue=>eid=>1 ekey.predicate)))
        ((merge-join fx< curie=>1.S subject=>eid=>object=>1)
         (lambda (id.S __ eid=>O=>1)
           (let* ((name.S (get-name-from-dict-safe (dict-get curie=>ckey=>cvalue=>1 id.S) ckey.name))
                  (S      (id->string id.S)))
             ((merge-join fx< predicate=>1 predicate=>eid=>1)
              (lambda (id.predicate.S->O __ eid=>1)
                (let ((predicate.S->O (id->string id.predicate.S->O)))
                  ((merge-join fx< eid=>1 eid=>O=>1)
                   (lambda (eid __ O=>1)
                     ((merge-join fx< curie=>1.O O=>1)
                      (lambda (id.O __ ___)
                        (let* ((name.O (get-name-from-dict-safe (dict-get curie=>ckey=>cvalue=>1 id.O)
                                                                ckey.name))
                               (O      (id->string id.O)))
                          (yield (list* S name.S predicate.S->O O name.O
                                        (edge-id->properties eid))))))))))))))))
    (define (query.obj->sub yield)
      (let* ((ekey.predicate    (string->id str.predicate))
             (ckey.name         (string->id "name"))
             (predicate=>1      (string*->id=>1 predicate*.S->O))
             (predicate=>eid=>1 (dict-get ekey=>evalue=>eid=>1 ekey.predicate)))
        ((merge-join fx< curie=>1.O object=>eid=>subject=>1)
         (lambda (id.O __ eid=>S=>1)
           (let* ((name.O (get-name-from-dict-safe (dict-get curie=>ckey=>cvalue=>1 id.O) ckey.name))
                  (O      (id->string id.O)))
             ((merge-join fx< predicate=>1 predicate=>eid=>1)
              (lambda (id.predicate.O->S __ eid=>1)
                (let ((predicate.O->S (id->string id.predicate.O->S)))
                  ((merge-join fx< eid=>1 eid=>S=>1)
                   (lambda (eid __ S=>1)
                     ((merge-join fx< curie=>1.S S=>1)
                      (lambda (id.S __ ___)
                        (let* ((name.S (get-name-from-dict-safe (dict-get curie=>ckey=>cvalue=>1 id.S)
                                                                ckey.name))
                               (S      (id->string id.S)))
                          (yield (list* S name.S predicate.O->S O name.O
                                        (edge-id->properties eid))))))))))))))))
    (cond
      [(eq? direction-tag 'obj->sub) (maybe-time (enumerator->rlist query.obj->sub))]
      [(eq? direction-tag 'sub->obj) maybe-time (enumerator->rlist query.sub->obj)]
      [else (error "unknown direction tag")]))

  (define (query:Concept curie*)
    (define (query yield)
      (let ((curie=>1 (string*->id=>1 curie*)))
        ((merge-join fx< curie=>1 curie=>ckey=>cvalue=>1)
         (lambda (id.curie _ ckey=>cvalue=>1)
           (let ((curie (id->string id.curie)))
             ((dict-enumerator ckey=>cvalue=>1)
              (lambda (id.key cvalue=>1)
                (yield (list curie (id->string id.key) (id->string (dict-min cvalue=>1)))))))))))
    (maybe-time (enumerator->list query)))

  (define (get-highest-bucket-number)
    (define (helper yield)
      ((dict-key-enumerator score=>pred=>sub=>obj=>eid=>1)
       (lambda (score)
         (yield (list score)))))
    (let* ((score* (remove-duplicates (enumerator->rlist helper)))
           (score* (sort (apply append score*) >)))
      (let loop ((s* score*))
        (if (or (eq? (car s*) 1111) (eq? (car s*) 1112))
            (loop (cdr s*))
            (car s*)))))

  (pretty-log `(defining db for)
              path.here
              db-path-under-parent)
  (define db (database (build-path path.here db-path-under-parent)))
  (pretty-log `(defined db for)
              path.here
              db-path-under-parent)


  (pretty-log `(defining r.cprop for)
              path.here
              db-path-under-parent)
  (define r.cprop (database-relation db 'cprop))
  (pretty-log `(defining r.edge for)
              path.here
              db-path-under-parent)  
  (define r.edge  (database-relation db 'edge))
  (pretty-log `(defining r.eprop for)
              path.here
              db-path-under-parent)
  (define r.eprop (database-relation db 'eprop))

  (define r.scored-edge (database-relation db 'scored-edge))  

  (pretty-log `(defining tcells for)
              path.here
              db-path-under-parent)
  (define tcell.text=>id (make-thread-cell #f))
  (define tcell.id=>text (make-thread-cell #f))

  (define (initialize-text!)
    (unless (thread-cell-ref tcell.text=>id)
      (define-values (text=>id id=>text) (relation-text-dicts r.cprop #f))
      (thread-cell-set! tcell.text=>id text=>id)
      (thread-cell-set! tcell.id=>text id=>text)))
  
  (pretty-log `(loading relation index dictionaries for db)
              path.here
              db-path-under-parent)
  (define subject=>object=>eid=>1 (maybe-time (relation-index-dict r.edge  '(subject object eid) #f))) ;; #f
  (define object=>subject=>eid=>1 (maybe-time (relation-index-dict r.edge  '(object subject eid) #f))) ;; #f
  (define subject=>eid=>object=>1 (maybe-time (relation-index-dict r.edge  '(subject eid object) #f))) ;; #t
  (define object=>eid=>subject=>1 (maybe-time (relation-index-dict r.edge  '(object eid subject) #f))) ;; #t
  (define ekey=>evalue=>eid=>1    (maybe-time (relation-index-dict r.eprop '(key value eid)      #f))) ;; #f
  (define eid=>ekey=>evalue=>1    (maybe-time (relation-index-dict r.eprop '(eid key value)      #f))) ;; #f
  (define ckey=>cvalue=>curie=>1  (maybe-time (relation-index-dict r.cprop '(key value curie)    #t))) ;; #t
  (define curie=>ckey=>cvalue=>1  (maybe-time (relation-index-dict r.cprop '(curie key value)    #f))) ;; #f
  (define score=>pred=>sub=>obj=>eid=>1 (maybe-time
                                         (relation-index-dict r.scored-edge '(score predicate subject object eid) #t)))
  (define score=>pred=>obj=>sub=>eid=>1 (maybe-time
                                         (relation-index-dict r.scored-edge '(score predicate object subject eid) #t)))
  (pretty-log `(loaded relation index dictionaries for db)
              path.here
              db-path-under-parent)
 
  (list
   query:Known->Known
   query:Known->X-scored
   query:Known->X
   query:X->Known-scored
   query:X->Known
   query:Known<-X->Known
   query:Known->X->Known
   query:X->Y->Known
   query:Concept
   concept-properties
   concept-property-values
   curie-in-db?
   curies-in-db
   curie->properties
   edge-properties
   edge-property-values
   edge-id->properties
   get-highest-bucket-number
   )
  )
