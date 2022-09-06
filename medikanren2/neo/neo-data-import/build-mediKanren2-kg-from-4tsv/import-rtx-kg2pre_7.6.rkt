#lang racket/base
(require "../../dbKanren/dbk/database.rkt"
         racket/list racket/pretty racket/runtime-path)

(define-runtime-path path.here ".")

(pretty-write `(current-batch-size: ,(current-batch-size)))

(define relation-specs '(
                         (eprop
                           "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/rtx-kg2pre_7.6/rtx-kg2pre_7.6.edgeprop.tsv"
                           (int text text)
                           (eid key value)
                           ((eid key value)
                            (key value eid)))
                         (edge
                          "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/rtx-kg2pre_7.6/rtx-kg2pre_7.6.edge.tsv"
                           (int text text)
                           (eid subject object)
                           ((eid)
                            (subject object)
                            (object  subject)
                            (subject eid object)
                            (object  eid subject)))
                         (cprop
                          "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/rtx-kg2pre_7.6/rtx-kg2pre_7.6.nodeprop.tsv"
                           (text text text)
                           (curie key value)
                           ((curie key value)
                            (key value curie)))
                         ))
(pretty-write `(relation-specs: . ,relation-specs))

(define db (database (build-path path.here "../../neo-data/rtx-kg2pre_7.6.db")))

(define relation-names   (map car             relation-specs))
(define relation-files   (map cadr            relation-specs))
(define relation-types   (map caddr           relation-specs))
(define relation-attrs   (map cadddr          relation-specs))
(define relation-indexes (map car (map cddddr relation-specs)))

(for-each (lambda (name file-name type attrs)
            (unless (database-relation-name? db name)
              (pretty-write `(building ,name from ,file-name))
              (define R (build-tsv-relation db type file-name))
              (relation-name-set! R name)
              (relation-attributes-set! R attrs)
              (database-commit! db)))
          relation-names
          relation-files
          relation-types
          relation-attrs)
(database-trash-empty! db)

(pretty-write '(compacting full database))
(for-each (lambda (name)
            (define R (database-relation db name))
            (relation-full-compact! R))
          relation-names)
(database-commit! db)
(database-trash-empty! db)

(for-each (lambda (name indexes)
            (define R (database-relation db name))
            (for-each (lambda (index)
                        (pretty-write `(indexing ,name for ,index))
                        (relation-index-add! R index)
                        (database-commit! db))
                      indexes))
          relation-names
          relation-indexes)
(database-trash-empty! db)
