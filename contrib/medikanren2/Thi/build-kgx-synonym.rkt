#lang racket/base
(require "../../../medikanren2/dbk/dbk/io.rkt"
         "../../../medikanren2/dbk/dbk/data.rkt"
         "../../../medikanren2/dbk/dbk/stream.rkt"
         racket/runtime-path)

(define-runtime-path path.here ".")

(define db (database (build-path path.here "kgx-synonym.db")))

(unless (database-relation-has? db '(kgx-synonym edge))
  (database-relation-add!
    db '(kgx-synonym edge)
    'attributes '(subject object source)
    'type       '(string  string string)
    'source     (s-map (lambda (row)
                         (map (lambda (key) (hash-ref row key))
                              '(subject object source_database)))
                       (in:file "KGX-NN-data/KGX_NN_data_2021-03-11_edges.jsonl"))))

(database-compact! db)

(define r.edge (database-relation db '(kgx-synonym edge)))

(relation-index-add! r.edge
                     '(subject object)
                     '(object  subject)
                     '(source))
