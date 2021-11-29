#lang racket/base
(require
  "../../../medikanren2/dbk/dbk/io.rkt"
  "../../../medikanren2/dbk/dbk/data.rkt"
  "../../../medikanren2/dbk/dbk/stream.rkt"
  racket/runtime-path)

(define-runtime-path path.here ".")

(define db (database (build-path path.here "rtx-kg2_20210204.db")))

(unless (database-relation-has? db '(rtx-kg2 cprop))
  (database-relation-add!
    db '(rtx-kg2 cprop)
    'attributes '(curie  key    value)
    'type       '(string string string)
    'source     (in:file "rtx_kg2.nodeprop.tsv" 'header '(":ID" "propname" "value"))))

(unless (database-relation-has? db '(rtx-kg2 edge))
  (database-relation-add!
    db '(rtx-kg2 edge)
    'attributes '(eid subject object)
    'type       '(nat string  string)
    'source     (s-map (lambda (row) (cons (string->number (car row)) (cdr row)))
                       (in:file "rtx_kg2.edge.tsv"     'header '(":ID" ":START" ":END")))))

(unless (database-relation-has? db '(rtx-kg2 eprop))
  (database-relation-add!
    db '(rtx-kg2 eprop)
    'attributes '(eid key    value)
    'type       '(nat string string)
    'source     (s-map (lambda (row) (cons (string->number (car row)) (cdr row)))
                       (in:file "rtx_kg2.edgeprop.tsv" 'header '(":ID" "propname" "value")))))

(define cprop (database-relation db '(rtx-kg2 cprop)))
(define edge  (database-relation db '(rtx-kg2 edge)))
(define eprop (database-relation db '(rtx-kg2 eprop)))

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
