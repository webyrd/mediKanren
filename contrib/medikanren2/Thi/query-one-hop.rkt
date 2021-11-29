#lang racket/base
(provide query-one-hop)
(require
  "../../../medikanren2/dbk/dbk/data.rkt"
  "../../../medikanren2/dbk/dbk/enumerator.rkt"
  racket/runtime-path)

;; query-one-hop is analogous to a miniKanren-style query with this shape:
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

(define (query-one-hop subject-categories predicates object-synonyms)
  (define (query yield)
    (define ekey.predicate.id         (dict-select dict.string=>id "predicate"))
    (define ckey.category.id          (dict-select dict.string=>id "category"))
    (define ckey.name.id              (dict-select dict.string=>id "name"))
    (define dict.object-synonyms      (strings->dict object-synonyms))
    (define dict.predicates           (strings->dict predicates))
    (define dict.subject-categories   (strings->dict subject-categories))
    (define dict.eprop.eid.predicate  (dict-select dict.eprop.eid.value.key   ekey.predicate.id))
    (define dict.cprop.curie.category (dict-select dict.cprop.curie.value.key ckey.category.id))
    ((merge-join dict.object-synonyms dict.edge.subject.eid.object)
     (lambda (id.o __ dict.edge.subject.eid)
       ;; TODO: try dict-join-ordered with dict.cprop.value.key.curie to retrieve oname
       (define id.oname ((dict-select (dict-select dict.cprop.value.key.curie id.o) ckey.name.id) 'min))
       ;; TODO: try dict-join-ordered with dict.id=>string
       (define oname (dict-select dict.id=>string id.oname))
       ;; TODO: try dict-join-ordered with dict.id=>string
       (define o (dict-select dict.id=>string id.o))
       ((merge-join dict.predicates dict.eprop.eid.predicate)
        (lambda (id.p __ dict.eprop.eid)
          ;; TODO: try dict-join-ordered with dict.id=>string
          (define p (dict-select dict.id=>string id.p))
          ((merge-join dict.eprop.eid dict.edge.subject.eid)
           (lambda (eid __ dict.edge.subject)
             ((merge-join dict.subject-categories dict.cprop.curie.category)
              (lambda (__ ___ dict.cprop.curie)
                ((dict-join-ordered
                   (lambda (yield)
                     ((merge-join dict.cprop.curie dict.edge.subject)
                      (lambda (id.s __ ___)
                        (yield id.s '()))))
                   dict.cprop.value.key.curie)
                 (lambda (id.s __ dict.cprop.value.key)
                   (define id.sname ((dict-select dict.cprop.value.key ckey.name.id) 'min))
                   (define sname    (dict-select dict.id=>string      id.sname))
                   ;; TODO: try dict-join-ordered with dict.id=>string
                   (define s        (dict-select dict.id=>string      id.s))
                   (yield (list s sname p o oname)))))))))))))
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
