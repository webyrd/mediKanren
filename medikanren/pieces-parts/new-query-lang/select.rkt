#lang racket

(require
  (only-in "query.rkt"
           query/graph edges/ranked)
  )

(define-syntax query
  (syntax-parser
    [(_ (select s:selector-exp ...+)
        (concepts c:concept-decl ...+)
        (edges e:edge-decl ...))
     #'(let ([res
              (query/graph
                ([c.id c.category-expr] ...)
                ([e.id e.predicate-list-expr])
                [e.subject e.id e.object]
                ...)])
         (query-runtime
           res
           (lambda (c.id ... e.id ...)
             (row-runtime
               (lambda () s)
               ...))))]))

; ranked-paths output format:
; (listof
;   (list score
;         (listof (cons subject object))))
(define (query-runtime query-result selection-f)

  )

(define (row-runtime . selector-thunks)
  (apply
    append
    (for/list ([t selector-thunks])
      (call-with-values t list))))


;(define (db concept-or-edge)
  ;)

;(define (property concept-or-edge symbol)
  ;)

;(define (curie concept)
  ;)

;(define (name concept)
  ;)

;(define (category concept)
  ;)

;(define (predicate edge)
  ;)

;(define (pubmed edge parts)
  ;)

(module+ main
  ; example query
  (query
    (select
      (db E) (curie D) (name D) (category D) (curie G) (name G)
      (predicate E) (pubmed E '(url keyword symbol)))
    (concepts
      [D drug]
      [G gene])
    (edges
      [E : D regulates G]))
  )

