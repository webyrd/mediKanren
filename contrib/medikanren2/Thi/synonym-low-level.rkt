#lang racket/base
(provide curie->synonyms curies->synonyms)
(require
  "../../../medikanren2/dbk/dbk/data.rkt"
  "../../../medikanren2/dbk/dbk/enumerator.rkt"
  racket/runtime-path
  racket/set)

(define (curie->synonyms curie) (curies->synonyms (list curie)))

(define (curies->synonyms curies)
  (define (ids->dict ids)
    (define vec.ids (list->vector (sort (set->list ids) <)))
    (dict:ordered (column:vector vec.ids) (column:const '()) 0 (vector-length vec.ids)))
  (define (step dict.curie dict.edge.Y.X)
    (enumerator->rlist
      (lambda (yield)
        ((merge-join dict.curie dict.edge.Y.X)
         (lambda (id.curie _ dict.edge.Y)
           ((dict.edge.Y 'enumerator) yield))))))
  (let loop ((current (set))
             (next    (list->set (enumerator->rlist ((strings->dict curies) 'enumerator)))))
    (let ((new (set-subtract next current)))
      (if (set-empty? new)
        (enumerator->list
          (lambda (yield)
            ((merge-join (ids->dict current) dict.id=>string)
             (lambda (_ __ curie) (yield curie)))))
        (let ((dict.new (ids->dict new)))
          (loop (set-union current new)
                (list->set (append (step dict.new dict.edge.object.subject)
                                   (step dict.new dict.edge.subject.object)))))))))

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;
(define (dict-select d key) (d 'ref key (lambda (v) v) (lambda () (error "dict ref failed" key))))

;; TODO: build small in-memory relations more easily
(define (strings->dict strs)
  (define vec.strs  (list->vector (sort (set->list (list->set strs)) string<?)))
  (define dict.strs (dict:ordered (column:vector vec.strs) (column:const '()) 0 (vector-length vec.strs)))
  (define vec.ids   (enumerator->vector
                      (lambda (yield)
                        ((merge-join dict.strs dict.string=>id)
                         (lambda (__ ___ id) (yield id))))))
  (dict:ordered (column:vector vec.ids) (column:const '()) 0 (vector-length vec.ids)))

(define-runtime-path path.here ".")
(define db     (database (path->string (build-path path.here "kgx-synonym.db"))))
(define r.edge (database-relation db '(kgx-synonym edge)))

(define domain-dicts                 (relation-domain-dicts r.edge))
(define dict.string=>id              (car (hash-ref (car domain-dicts) 'text)))
(define dict.id=>string              (car (hash-ref (cdr domain-dicts) 'text)))

(define dict.edge.object.subject (relation-index-dict r.edge '(subject object)))
(define dict.edge.subject.object (relation-index-dict r.edge '(object subject)))
