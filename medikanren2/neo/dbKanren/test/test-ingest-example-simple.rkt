#lang racket/base
(require "../dbk/io.rkt"
         "../dbk/data.rkt"
         "../dbk/stream.rkt"
         racket/runtime-path)

(define-runtime-path path.here ".")

(define db (database (build-path path.here "example-db")))

(unless (database-relation-has? db '(example cprop))
  (database-relation-add!
    db '(example cprop)
    'attributes '(curie  key    value)
    'type       '(string string string)
    'source     (in:file "example/example.nodeprop.tsv" 'header '(":ID" "propname" "value"))))

(unless (database-relation-has? db '(example edge))
  (database-relation-add!
    db '(example edge)
    'attributes '(eid subject object)
    'type       '(nat string  string)
    'source     (s-map (lambda (row) (cons (string->number (car row)) (cdr row)))
                       (in:file "example/example.edge.tsv"     'header '(":ID" ":START" ":END")))))

(unless (database-relation-has? db '(example eprop))
  (database-relation-add!
    db '(example eprop)
    'attributes '(eid key    value)
    'type       '(nat string string)
    'source     (s-map (lambda (row) (cons (string->number (car row)) (cdr row)))
                       (in:file "example/example.edgeprop.tsv" 'header '(":ID" "propname" "value")))))

(define cprop (database-relation db '(example cprop)))
(define edge  (database-relation db '(example edge)))
(define eprop (database-relation db '(example eprop)))

(database-compact! db)

(relation-index-add! cprop
                     '(curie key)
                     '(key value))
(relation-index-add! edge
                     '(eid)
                     '(subject object)
                     '(subject eid)
                     '(object  eid))
(relation-index-add! eprop
                     '(eid key)
                     '(key value))
