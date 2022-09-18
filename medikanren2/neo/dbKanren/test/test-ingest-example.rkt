#lang racket/base
(require "../dbk/io.rkt"
         racket/match)

;; TODO: require these
(define file-checksum          #f)
(define-syntax-rule (run* body ...) '(TODO: run* body ...))
(define database               #f)
(define database-relation      #f)
(define database-update!       #f)
(define database-checkpoint!   #f)
(define database-index-build!  #f)
(define database-compact!      #f)
(define relation-database      #f)
(define relation-name          #f)
(define relation-insert        #f)
(define relation-index-add!    #f)
(define relation-index-remove! #f)
(define relation-index-build!  #f)

(define db (database "path/to/example/db"
                     'immediate-checkpoint?  #f
                     'immediate-index-build? #f
                     'immediate-compact?     #f))

(define update-history (database-relation
                         db
                         'name       'update-history
                         'attributes '(relation-name update-time update-kind file-name file-size file-checksum)
                         'type       '(#f            nat         #f          string    nat       string)))
(database-checkpoint! db)

(define cprop (database-relation
                db '(example cprop)
                'attributes '(curie  key    value)
                'type       '(string string string)))
(define eprop (database-relation
                db '(example eprop)
                'attributes '(eid key    value)
                'type       '(nat string string)))
(define edge  (database-relation
                db '(example edge)
                'attributes '(eid subject object)
                'type       '(nat string  string)))
(relation-index-add! cprop
                     '(curie key)
                     '(key value))
(relation-index-add! eprop
                     '(eid key)
                     '(key value))
(relation-index-add! edge
                     '(eid)
                     '(subject object)
                     '(object subject))
(database-checkpoint! db)

(define (relation-ingest! R path.dir file-name file-params)
  (define name.R              (relation-name R))
  (define path.file           (build-path path.dir file-name))
  (define checksum.file       (file-checksum path.file))
  (define size.file           (file-size path.file))
  (define file-ingest-history (run* (timestamp size checksum)
                                (update-history name.R timestamp 'ingest file-name size checksum)))
  (if (null? file-ingest-history)
    (begin (database-update!
             (relation-database R)
             (relation-insert update-history (list (list name.R (current-milliseconds)
                                                         'ingest file-name size.file checksum.file)))
             (relation-insert R              (apply in:file path.file file-params)))
           (database-checkpoint! db))
    (unless (ormap (lambda (entry) (match entry
                                     ((list timestamp size checksum) (and (equal? size     size.file)
                                                                          (equal? checksum checksum.file)))
                                     (_                              #f)))
                   file-ingest-history)
      (error "inconsistent data ingestion history:"
             'relation-name name.R
             'file-name     file-name
             'file-size     size.file
             'file-checksum checksum.file
             'history       file-ingest-history))))

(relation-ingest! cprop "example" "example.nodeprop.tsv" 'header '(":ID" "propname" "value"))
(relation-ingest! eprop "example" "example.edgeprop.tsv" 'header '(":ID" "propname" "value"))
(relation-ingest! edge  "example" "example.edge.tsv"     'header '(":ID" ":START" ":END"))

(database-index-build! db)
(database-compact!     db)
