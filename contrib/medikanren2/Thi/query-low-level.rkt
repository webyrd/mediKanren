#lang racket/base
(provide query:X->Known query:Known<-X->Known)
(require
  "../../../medikanren2/dbk/dbk/data.rkt"
  "../../../medikanren2/dbk/dbk/enumerator.rkt"
  "../../../medikanren2/dbk/dbk/stream.rkt"
  racket/match
  racket/runtime-path)

;; query:X->Known is analogous to a miniKanren-style query with this shape:
; (run* (s sname p o oname)
;   (fresh (id category)
;     (edge id s o)
;     (cprop s "category" category)
;     (cprop s "name" sname)
;     (cprop o "name" oname)
;     (eprop id "predicate" p)
;     (membero o object-curies)
;     (membero p predicates)
;     (membero category subject-categories)))

(define (query:X->Known categories.X predicates.X->K curies.K)
  (define (query yield)
    (define ekey.predicate.id         (dict-select dict.string=>id "predicate"))
    (define ckey.category.id          (dict-select dict.string=>id "category"))
    (define ckey.name.id              (dict-select dict.string=>id "name"))
    (define dict.categories.X         (strings->dict categories.X))
    (define dict.predicates.X->K      (strings->dict predicates.X->K))
    (define dict.curies.K             (strings->dict curies.K))
    (define dict.eprop.eid.predicate  (dict-select dict.eprop.eid.value.key   ekey.predicate.id))
    (define dict.cprop.curie.category (dict-select dict.cprop.curie.value.key ckey.category.id))
    ((merge-join dict.curies.K dict.edge.subject.eid.object)
     (lambda (id.K __ dict.edge.X.eid)
       (define id.name.K ((dict-select (dict-select dict.cprop.value.key.curie id.K) ckey.name.id) 'min))
       (define name.K    (dict-select dict.id=>string id.name.K))
       (define K         (dict-select dict.id=>string id.K))
       ((merge-join dict.predicates.X->K dict.eprop.eid.predicate)
        (lambda (id.predicate.X->K __ dict.eprop.X->K)
          (define predicate.X->K (dict-select dict.id=>string id.predicate.X->K))
          ((merge-join dict.eprop.X->K dict.edge.X.eid)
           (lambda (eid.X->K __ dict.edge.X)
             ((merge-join dict.categories.X dict.cprop.curie.category)
              (lambda (__ ___ dict.cprop.X)
                ((dict-join-ordered
                   (lambda (yield)
                     ((merge-join dict.cprop.X dict.edge.X)
                      (lambda (id.X __ ___)
                        (yield id.X '()))))
                   dict.cprop.value.key.curie)
                 (lambda (id.X __ dict.cprop.value.key)
                   (define id.name.X ((dict-select dict.cprop.value.key ckey.name.id) 'min))
                   (define name.X    (dict-select dict.id=>string id.name.X))
                   (define X         (dict-select dict.id=>string id.X))
                   (yield (list X name.X predicate.X->K K name.K)))))))))))))
  (time (enumerator->rlist query)))

;; query:Known<-X->Known is analogous to a miniKanren-style query with this shape:
;(run* (K1 name.K1 predicates.K1<-X X name.X predicates.X->K1 K2 name.K2)
;  (fresh (id1 id2 category.X)
;    (edge id1 X K1)
;    (edge id2 X K2)
;    (cprop X   "category" category.X)
;    (cprop X   "name" name.X)
;    (cprop K1  "name" name.K1)
;    (cprop K2  "name" name.K2)
;    (eprop id1 "predicate" K1<-X)
;    (eprop id2 "predicate" X->K2)
;    (membero category.X categories.X)
;    (membero K1         curies.K1)
;    (membero K1<-X      predicates.K1<-X)
;    (membero K2         curies.K2)
;    (membero X->K2      predicates.X->K2)))

(define (query:Known<-X->Known curies.K1 predicates.K1<-X categories.X predicates.X->K2 curies.K2)
  (define (candidates->dict candidates)
    (define ordered (sort candidates (lambda (a b) (string<? (car a) (car b)))))
    (define groups  (s-group ordered equal? car))
    (dict:ordered:vector (list->vector groups) caar))
  (define candidates.X&name&X->K1&K1&name (query:X->Known categories.X predicates.K1<-X curies.K1))
  (define candidates.X&name&X->K2&K2&name (query:X->Known categories.X predicates.X->K2 curies.K2))
  (define dict.XK1s.X                     (candidates->dict candidates.X&name&X->K1&K1&name))
  (define dict.XK2s.X                     (candidates->dict candidates.X&name&X->K2&K2&name))
  (time (enumerator->list
          (lambda (yield)
            ((merge-join dict.XK1s.X dict.XK2s.X)
             (lambda (X XK1s XK2s)
               (for-each (lambda (XK1)
                           (match-define (list _ name.X X->K1 K1 name.K1) XK1)
                           (for-each (lambda (XK2)
                                       (match-define (list _ _ X->K2 K2 name.K2) XK2)
                                       (yield (list K1 name.K1 X->K1 X name.X X->K2 K2 name.K2)))
                                     XK2s))
                         XK1s)))))))

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;
(define (dict-select d key) (d 'ref key (lambda (v) v) (lambda () (error "dict ref failed" key))))

;; TODO: build small in-memory relations more easily
(define (strings->dict strs)
  (define vec.strs  (list->vector (sort strs string<?)))
  (define dict.strs (dict:ordered (column:vector vec.strs) (column:const '()) 0 (vector-length vec.strs)))
  (define vec.ids   (enumerator->vector
                      (lambda (yield)
                        ((merge-join dict.strs dict.string=>id)
                         (lambda (__ ___ id) (yield id))))))
  (dict:ordered (column:vector vec.ids) (column:const '()) 0 (vector-length vec.ids)))

(define-runtime-path path.here ".")
(define db      (database (path->string (build-path path.here "rtx-kg2_20210204.db"))))
(define r.cprop (database-relation db '(rtx-kg2 cprop)))
(define r.edge  (database-relation db '(rtx-kg2 edge)))
(define r.eprop (database-relation db '(rtx-kg2 eprop)))

(define domain-dicts                 (relation-domain-dicts r.cprop))
(define dict.string=>id              (car (hash-ref (car domain-dicts) 'text)))
(define dict.id=>string              (car (hash-ref (cdr domain-dicts) 'text)))

(define dict.edge.subject.eid.object (relation-index-dict r.edge  '(object eid subject)))
(define dict.eprop.eid.value.key     (relation-index-dict r.eprop '(key value eid)))
(define dict.cprop.curie.value.key   (relation-index-dict r.cprop '(key value curie)))
(define dict.cprop.value.key.curie   (relation-index-dict r.cprop '(curie key value)))
