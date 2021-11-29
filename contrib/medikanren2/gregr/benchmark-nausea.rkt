#lang racket/base
(require
  "../../../medikanren2/dbk/dbk.rkt"
  "../../../medikanren2/dbk/dbk/data.rkt"
  "../../../medikanren2/dbk/dbk/enumerator.rkt"
  racket/pretty
  racket/runtime-path
  racket/set)

(define-relation/table (cprop curie key value)   'path "../../../medikanren2/data/rtx2/20210204/cprop")
(define-relation/table (edge  id subject object) 'path "../../../medikanren2/data/rtx2/20210204/edge")
(define-relation/table (eprop id key value)      'path "../../../medikanren2/data/rtx2/20210204/eprop")

(define (dict-select d key) (d 'ref key (lambda (v) v) (lambda () (error "dict ref failed" key))))

(define-runtime-path path.here ".")

(define db      (database (path->string (build-path path.here "rtx-kg2_20210204.db"))))
(define r.cprop (database-relation db '(rtx-kg2 cprop)))
(define r.edge  (database-relation db '(rtx-kg2 edge)))
(define r.eprop (database-relation db '(rtx-kg2 eprop)))

(define dict.eprop.eid.value.key     (relation-index-dict r.eprop '(key value eid)))
(define dict.edge.subject.eid.object (relation-index-dict r.edge  '(object eid subject)))
(define dict.cprop.value.key.curie   (relation-index-dict r.cprop '(curie key value)))
(define domain-dicts                 (relation-domain-dicts r.cprop))
(define dict.string=>id              (car (hash-ref (car domain-dicts) 'text)))
(define dict.id=>string              (car (hash-ref (cdr domain-dicts) 'text)))

(define (string->id str) (dict-select dict.string=>id str))
(define (id->string id)  (dict-select dict.id=>string id))

(define (benchmark-find-treatments curie.target)
  (define (run-query yield)
    (define curie.nausea.id       (string->id curie.target))
    (define ekey.predicate.id     (string->id "predicate"))
    (define evalue.treats.id      (string->id "biolink:treats"))
    (define ckey.category.id      (string->id "category"))
    (define ckey.name.id          (string->id "name"))
    (define dict.eprop.eid.value  (dict-select dict.eprop.eid.value.key     ekey.predicate.id))
    (define dict.eprop.eid        (dict-select dict.eprop.eid.value         evalue.treats.id))
    (define dict.edge.subject.eid (dict-select dict.edge.subject.eid.object curie.nausea.id))
    ((merge-join dict.eprop.eid dict.edge.subject.eid)
     (lambda (eid __ dict.edge.subject)
       ((merge-join dict.edge.subject dict.cprop.value.key.curie)
        (lambda (subject.id __ dict.cprop.value.key)
          (define subject             (id->string subject.id))
          (define dict.cprop.category (dict-select dict.cprop.value.key ckey.category.id))
          (define dict.cprop.name     (dict-select dict.cprop.value.key ckey.name.id))
          ((merge-join dict.cprop.category dict.id=>string)
           (lambda (category.id __ category)
             ((merge-join dict.cprop.name dict.id=>string)
              (lambda (name.id __ name)
                (yield (list subject category name)))))))))))
  ;; Some nausea timings
  ;; cpu time: 1485 real time: 1610 gc time: 19
  ;; cpu time: 1539 real time: 1557 gc time: 15
  ;; cpu time: 1538 real time: 1556 gc time: 24
  (define results.old (time (run* (s cat name)
                              (fresh (eid)
                                (edge eid s curie.target)
                                (cprop s "category" cat)
                                (cprop s "name" name)
                                (eprop eid "predicate" "biolink:treats")))))
  ;; Some nausea timings
  ;; cpu time: 27 real time: 27 gc time: 0
  ;; cpu time: 31 real time: 31 gc time: 0
  ;; cpu time: 30 real time: 31 gc time: 0
  (define results.new (time (enumerator->rlist run-query)))
  ;; 149 results
  (pretty-write `(old:    ,(length results.old) ,results.old))
  (pretty-write `(new:    ,(length results.new) ,results.new))
  (pretty-write `(equal?: ,(equal? (list->set results.old) (list->set results.new)))))

(define curie.nausea "UMLS:C0520909")
(benchmark-find-treatments curie.nausea)
