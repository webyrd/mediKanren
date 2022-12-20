#lang racket/base
(provide
  curie->synonyms
  curies->synonyms)
(require
  "../dbKanren/dbk/data.rkt"
  "../dbKanren/dbk/enumerator.rkt"
  racket/runtime-path
  racket/set)

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
  (define (ids->dict ids)
    (define vec.ids (list->vector (sort (set->list ids) <)))
    (dict:ordered (column:vector vec.ids) (column:const '()) 0 (vector-length vec.ids)))
  (define (step new)
    (define (step/dict dict.edge.Y.X) (enumerator->rlist
                                        (lambda (yield)
                                          ((merge-join dict.new dict.edge.Y.X)
                                           (lambda (id.curie _ dict.edge.Y)
                                             ((dict.edge.Y 'enumerator) yield))))))
    (define dict.new (ids->dict new))
    (list->set (append (step/dict dict.edge.object.subject)
                       (step/dict dict.edge.subject.object))))
  (define ids.final (set-fixed-point (list->set (strings->ids curies)) step))
  (initialize-text!)
  (let ((dict.id=>string (thread-cell-ref tcell.id=>string)))
    (set->list
     (set-union
      (list->set curies)
      (list->set
       (enumerator->list
        (lambda (yield)
          ((merge-join (ids->dict ids.final) dict.id=>string)
           (lambda (_ __ curie) (yield curie))))))))))

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;
(define (set-fixed-point xs.initial step)
  (let loop ((current (set))
             (next    xs.initial))
    (let ((new (set-subtract next current)))
      (if (set-empty? new)
        current
        (loop (set-union current new)
              (step      new))))))

(define (dict-select d key) (d 'ref key (lambda (v) v) (lambda () (error "dict ref failed" key))))

;; TODO: build small in-memory relations more easily
(define (strings->dict strs)
  (define vec.strs  (list->vector (sort (set->list (list->set strs)) string<?)))
  (define dict.strs (dict:ordered (column:vector vec.strs) (column:const '()) 0 (vector-length vec.strs)))
  (initialize-text!)
  (define vec.ids (let ((dict.string=>id (thread-cell-ref tcell.string=>id)))
                    (enumerator->vector
                      (lambda (yield)
                        ((merge-join dict.strs dict.string=>id)
                         (lambda (__ ___ id) (yield id)))))))
  (dict:ordered (column:vector vec.ids) (column:const '()) 0 (vector-length vec.ids)))

(define (strings->ids strs) (enumerator->rlist ((strings->dict strs) 'enumerator)))

(define-runtime-path path.here "../neo-data")
(define db     (database (path->string (build-path path.here "kgx-synonym/old-neo/kgx-synonym.db"))))
(define r.edge (database-relation db '(kgx-synonym edge)))

(define tcell.string=>id (make-thread-cell #f))
(define tcell.id=>string (make-thread-cell #f))

(define (initialize-text!)
  (unless (thread-cell-ref tcell.string=>id)
    (let* ((domain-dicts    (relation-domain-dicts r.edge #f))
           (dict.string=>id (car (hash-ref (car domain-dicts) 'text)))
           (dict.id=>string (car (hash-ref (cdr domain-dicts) 'text))))
      (thread-cell-set! tcell.string=>id dict.string=>id)
      (thread-cell-set! tcell.id=>string dict.id=>string))))

(define dict.edge.object.subject (time (relation-index-dict r.edge '(subject object) #t)))
(define dict.edge.subject.object (time (relation-index-dict r.edge '(object subject) #t)))
