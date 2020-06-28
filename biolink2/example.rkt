#lang racket/base
(require "dbk/dbk.rkt" racket/function racket/pretty racket/string)

(define buffer-size 10000)  ;; main memory used for external sorting

(define argv (current-command-line-arguments))
(define argv-expected '#(DATA_DIR DB_NAME INPUT_SUFFIX))
(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error "command line argument mismatch:" argv-expected argv))

(define data-dir    (vector-ref argv 0))
(define db-name     (vector-ref argv 1))
(define file-suffix (vector-ref argv 2))
(define db-dir
  (path->string (expand-user-path (build-path data-dir db-name))))
(define (db-path fname)
  (path->string (expand-user-path (build-path db-dir fname))))

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
(define (validate-header header-expected in)
  (define header-found (read-line in 'any))
  (when (not (equal? header-found (string-join header-expected
                                               header-delimiter)))
    (error "unexpected header:" header-found header-expected)))

(define (materialize-dsv-stream in header . mat-args)
  (let ((mat (apply materializer mat-args)))
    (validate-header header in)
    (define count 0)
    (time (begin (s-each (dsv->stream in)
                         (lambda (x)
                           (when (= 0 (remainder count 100000))
                             (printf "Ingested ~s rows\n" count))
                           (mat 'put x)
                           (set! count (+ count 1))))
                 (printf "Processing ingesting ~s rows\n" count)
                 (mat 'close)
                 (printf "Finished processing ~s rows\n" count)))))

;; materialize a relation if not present
(define (materialize-relation name fnin header fields types)
  (unless (directory-exists? (db-path name))
    (printf "buildling relation: ~s; ~s\n"
            (db-path fnin) (db-path name))
    (let/files ((in (db-path fnin))) ()
      (materialize-dsv-stream
        in header
	;; input columns
	fields buffer-size (db-path name)
	;; attribute names, types, and internal key name for indexing
	fields types 'id
	;; primary table columns (without any sorted-columns)
	`((,fields . ())
	  ;; index table columns (without any sorted-columns)
	  (,(append (cdr fields) (list 'id)) . ()))))))

(materialize-relation "concept" fnin.nodeprop header.nodeprop
		      '(curie property value)
		      '(string string string))

(materialize-relation "predicate" fnin.edge header.edge
		      '(:id :start :end)
		      '(string string string))

#;
(materialize-relation "predicateprop" fnin.edgeprop header.edgeprop
		      '(:id property value)
		      '(string string string))
#|
Does not work
...
Ingested 134100000 rows
<: contract violation
  expected: real?
  given: #<void>
  context...:
   condition->exn
   do-raise
   dynamic-wind
   /Users/namin/code/rel/mediKanren2/biolink2/dbk/codec.rkt:115:0: encode-nat
   /Users/namin/code/rel/mediKanren2/biolink2/dbk/method.rkt:25:28
   /Users/namin/code/rel/mediKanren2/biolink2/example.rkt:48:25
   /Users/namin/code/rel/mediKanren2/biolink2/dbk/stream.rkt:48:0: s-each
   /Applications/Racket v7.7/collects/racket/private/more-scheme.rkt:336:52
   time-apply
   /Users/namin/code/rel/mediKanren2/biolink2/example.rkt:43:0: materialize-dsv-stream
   call-with-input-file
   call-with-values
   call-in-empty-metacontinuation-frame
   body of "/Users/namin/code/rel/mediKanren2/biolink2/example.rkt"
   for-loop
   run-module-instance!
|#

(time (let ()
        ;; baseline
        (define-materialized-relation concept 'disk  (db-path "concept"))
	(define-materialized-relation predicate 'disk  (db-path "predicate"))
	;;(define-materialized-relation predicateprop 'disk  (db-path "predicateprop"))
        ;; ~4x faster retrieval; ~400x slower loading
        ;(define-materialized-relation concept 'bytes (db-path "concept"))
        ;; ~10x faster retrieval; ~6000x slower loading
        ;(define-materialized-relation concept 'scm   (db-path "concept"))
        (time (pretty-print
	       (run 10 (curie1 k1 v1 curie2 k2 v2)
		    (fresh (id)
		       ;;(predicateprop id "edge_label" "biolink:has_gene_product")
		       (predicate id curie1 curie2)
		       (concept curie1 k1 v1)
		       (concept curie2 k2 v2)))))
        (newline)

        (time (pretty-print
                (run 600 (curie k v)
                  (concept curie "category" "chemical_substance")
                  (concept curie k v))))
        (newline)
        ))
