#lang racket/base
(provide query:X->Known)
(require
  "../../../medikanren2/dbk/dbk/data.rkt"
  "../../../medikanren2/dbk/dbk/enumerator.rkt"
  racket/runtime-path)

;; query:X->Known is analogous to a miniKanren-style query with this shape:
; (run* (s sname p o oname)
;   (fresh (id category)
;     (edge id s o)
;     (cprop s "category" category)
;     (cprop s "name" sname)
;     (cprop o "name" oname)
;     (eprop id "predicate" p)
;     (membero o object-synonyms)
;     (membero p predicates)
;     (membero category subject-categories)))

(define (query:X->Known categories.X predicates.X->K synonyms.K)
  (define (query yield)
    (define ekey.predicate.id         (dict-select dict.string=>id "predicate"))
    (define ckey.category.id          (dict-select dict.string=>id "category"))
    (define ckey.name.id              (dict-select dict.string=>id "name"))
    (define dict.categories.X         (strings->dict categories.X))
    (define dict.predicates.X->K      (strings->dict predicates.X->K))
    (define dict.synonyms.K           (strings->dict synonyms.K))
    (define dict.eprop.eid.predicate  (dict-select dict.eprop.eid.value.key   ekey.predicate.id))
    (define dict.cprop.curie.category (dict-select dict.cprop.curie.value.key ckey.category.id))
    ((merge-join dict.synonyms.K dict.edge.subject.eid.object)
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
