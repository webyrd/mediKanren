#lang racket/base
(provide

  )

(require
  "common.rkt"
  )

(define-syntax define-record
  (syntax-rules ()
    ((_ name field ...) (struct name (field ...) #:prefab))))
(define-syntax define-record*
  (syntax-rules ()
    ((_ (record-body ...) ...)
     (begin (define-record record-body ...) ...))))

(define-record*
  (setdesc kind sources construction class text selections)
  ;; kind: whether this is a concept or predicate set
  ;; sources: choice of databases
  ;; class: concept categories or relation parent classes
  ;; construction: in terms of operations on other sets
  ;; text: text search strings
  ;; selections: explicitly chosen elements (referenced by CURIE/db-uid)

  (metadata  uid attrs)
  (workspace meta graph uid=>obj triples)
  (graph     meta nodes edges subgraphs)
  (node      meta concepts edges)
  (edge      meta subject object predicates))

;; maybe just represent these as s-expressions
;(struct set-operation (op s1 s2))
;(struct constraint    (cx s arg))
;; category/class constraint
;; text search constraint
;; graph constraint
;; user selection constraint

;; TODO:
;; workspace manipulators
;;   track deltas to minimize recomputation of unknowns?
;; computate unknowns/triples for workspace graphs

;; union, intersection, difference

(define fresh-uid (let ((uid -1)) (lambda () (set! uid (+ uid 1)) uid)))

;(define (compute w)
  ;(define uid=>obj (workspace-uid=>obj w))
  ;(define seen     (set))
  ;;(define pending  (set))
  ;(define finished (hash))
  ;(define failed   (set))

  ;;(define unknowns '(()))

  ;(define (uid->obj uid) (hash-ref uid=>obj uid))

  ;(define (fail! obj->metadata obj)
    ;(set! failed (set-add failed (obj->metadata obj)))
    ;#f)

  ;(define (seen?/add obj->metadata obj)
    ;(define uid (metadata-uid (obj->metadata obj)))
    ;(or (set-member? seen uid)
        ;(begin (set! seen (set-add seen uid)) #f)))

  ;(define (finished? uid) (hash-ref? finished uid #f))

  ;(define (compute/graph g)
    ;(or (seen?/add graph-meta g)
        ;(unless
          ;(and (for-each compute/node  (map uid->obj (graph-nodes g)))
               ;(for-each compute/edge  (map uid->obj (graph-edges g)))
               ;(for-each compute/graph (map uid->obj (graph-subgraphs g)))
               ;(andmap (finished? node-meta)  (graph-nodes g))
               ;(andmap (finished? edge-meta)  (graph-edges g))
               ;(andmap (finished? graph-meta) (graph-subgraphs g)))
          ;(fail! graph-meta g))))

  ;(define (compute/edge e (query #f))
    ;(unless (and (compute/predicates

                   ;)

              ;(compute/node (uid->obj subject))
                 ;(compute/node (uid->obj object))
                 ;((finished? node-meta) subject)
                 ;((finished? node-meta) object))
      ;(fail! edge-meta e)))

  ;(define (compute/node n (query #f))
    ;(define sd (node-concepts n))
    ;(or (seen?/add node-meta n)
        ;;; TODO: unknown or constructed?
        ;(if (setdesc-construction sd)
          ;;; TODO: construct concept set.

          ;)

        ;))



  ;(define (compute/query

            ;)

    ;)

  ;(compute/graph (workspace-graph w))


  ;)
