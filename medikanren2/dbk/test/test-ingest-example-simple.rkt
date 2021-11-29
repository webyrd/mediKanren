#lang racket/base
(require "../dbk/io.rkt"
         racket/match)

;; TODO: require these
(define-syntax-rule (run* body ...) '(TODO: run* body ...))
(define database               #f)
(define database-relation-add! #f)
(define database-build!        #f)
(define relation-index-add!    #f)

(define db (database 'path "path/to/example/db"))

(define cprop (database-relation-add!
                db
                'name       '(example cprop)
                'attributes '(curie  key    value)
                'type       '(string string string)
                'source     (in:file "example/example.nodeprop.tsv" 'header '(":ID" "propname" "value"))))
(define eprop (database-relation-add!
                db
                'name       '(example eprop)
                'attributes '(eid key    value)
                'type       '(nat string string)
                'source     (in:file "example/example.edgeprop.tsv" 'header '(":ID" "propname" "value"))))
(define edge  (database-relation-add!
                db
                'name       '(example edge)
                'attributes '(eid subject object)
                'type       '(nat string  string)
                'source     (in:file "example/example.edge.tsv"     'header '(":ID" ":START" ":END"))))

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

(database-build! db)
