#lang racket/base
(require "common.rkt" racket/pretty)

(define argv (current-command-line-arguments))
(define argv-expected '#(DB_NAME INPUT_SUFFIX))
(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error "command line argument mismatch:" argv-expected argv))

(define db-name     (vector-ref argv 0))
(define file-suffix (vector-ref argv 1))
(define (db-relative-path path) (path->string (build-path db-name path)))
(define (db-path path) (path/data (db-relative-path path)))

(define dsv->stream (case file-suffix
                      (("csv") csv->stream)
                      (("tsv") tsv->stream)
                      (else (error "invalid file suffix:" file-suffix))))
(define header-delimiter (case file-suffix
                           (("csv") ",")
                           (("tsv") "\t")
                           (else (error "invalid file suffix:" file-suffix))))

;; Input
(define fnin.node     (string-append db-name ".node."     file-suffix))
(define fnin.nodeprop (string-append db-name ".nodeprop." file-suffix))
(define fnin.edge     (string-append db-name ".edge."     file-suffix))
(define fnin.edgeprop (string-append db-name ".edgeprop." file-suffix))
(define header.node     '(":ID"))
(define header.nodeprop '(":ID" "propname" "value"))
(define header.edgeprop '(":ID" "propname" "value"))
(define header.edge     '(":ID" ":START"   ":END"))

(define (materialize-db-relation
          name fnin header transform fields types indexes)
  (materialize-relation
    (db-relative-path name) (db-relative-path fnin) header header-delimiter
    dsv->stream transform fields types indexes))

(materialize-db-relation "concept" fnin.nodeprop header.nodeprop #f
                         '(curie property value)
                         '(string string string)
                         '((value property)))

(materialize-db-relation "edge" fnin.edge header.edge #f
                         '(:id :start :end)
                         '(string string string)
                         '((:start)  ;; (:start :end) doesn't work well now
                           (:end)))

(materialize-db-relation "edge-prop" fnin.edgeprop header.edgeprop #f
                         '(:id property value)
                         '(string string string)
                         '((value property)))

(time (let ()
        ;; ~4x faster retrieval; ~400x slower loading
        ;(define-materialized-relation concept   `((path . ,(db-path "concept")) (retrieval-type . bytes)))
        ;; ~10x faster retrieval; ~6000x slower loading
        ;(define-materialized-relation concept   `((path . ,(db-path "concept")) (retrieval-type . scm)))
        ;; baseline; including (retrieval-type . disk) is optional
        (define-materialized-relation concept   `((path . ,(db-path "concept"))))
        (define-materialized-relation edge      `((path . ,(db-path "edge"))))
        (define-materialized-relation edge-prop `((path . ,(db-path "edge-prop"))))
        (time (pretty-print
               (run 10 (curie1 predicate curie2)
                    (fresh (eid)
                       (edge-prop eid "edge_label" predicate)
                       (edge eid curie1 curie2)))))
        (newline)

        (time (pretty-print
               (run 10 (curie1 k1 v1 curie2 k2 v2)
                    (fresh (id)
                       (edge-prop id "edge_label" "biolink:has_gene_product")
                       (edge id curie1 curie2)
                       (concept curie1 k1 v1)
                       (concept curie2 k2 v2)))))
        (newline)

        (time (pretty-print
                (run 600 (curie name)
                  (concept curie "category" "chemical_substance")
                  (concept curie "name" name))))
        (newline)
        ))
