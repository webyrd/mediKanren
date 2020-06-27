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

(define fnin->stream (case file-suffix
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

;; materialize the (concept curie property value) relation if not present
(unless (directory-exists? (db-path "concept"))
  (printf "buildling relation: ~s; ~s\n"
          (db-path fnin.nodeprop) (db-path "concept"))
  (let/files ((in (db-path fnin.nodeprop))) ()
    (let ((mat (materializer
                 ;; input columns
                 '(curie property value) buffer-size (db-path "concept")
                 ;; attribute names, types, and internal key name for indexing
                 '(curie property value) '(string string string) 'id
                 ;; primary table columns (without any sorted-columns)
                 '(((curie property value) . ())
                   ;; index table columns (without any sorted-columns)
                   ((property value id)    . ())))))
      (validate-header header.nodeprop in)
      (define count 0)
      (time (begin (s-each (fnin->stream in)
                           (lambda (x)
                             (when (= 0 (remainder count 10000))
                               (printf "Ingested ~s rows\n" count))
                             (mat 'put x)
                             (set! count (+ count 1))))
                   (printf "Processing ingesting ~s rows\n" count)
                   (mat 'close)
                   (printf "Finished processing ~s rows\n" count))))))

(time (let ()
        ;; baseline
        (define-materialized-relation concept 'disk  (db-path "concept"))
        ;; ~4x faster retrieval; ~400x slower loading
        ;(define-materialized-relation concept 'bytes (db-path "concept"))
        ;; ~10x faster retrieval; ~6000x slower loading
        ;(define-materialized-relation concept 'scm   (db-path "concept"))
        (time (pretty-print
                (run 10 (curie k v)
                  (concept curie k v))))
        (newline)

        (time (pretty-print
                (run 600 (curie k v)
                  (concept curie "category" "chemical_substance")
                  (concept curie k v))))
        (newline)
        ))
