#lang racket/base
(require "dbk/dbk.rkt"
         (except-in racket/match ==)
         racket/function racket/list racket/pretty racket/string)

;; racket ex_cohd.rkt data cohd-v2
(define buffer-size 100000)  ;; main memory used for external sorting

(define argv (current-command-line-arguments))
(define argv-expected '#(DATA_DIR DB_NAME))
(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error "command line argument mismatch:" argv-expected argv))

(define data-dir (vector-ref argv 0))
(define db-name  (vector-ref argv 1))
(define db-dir
  (path->string (expand-user-path (build-path data-dir db-name))))
(define (db-path fname)
  (path->string (expand-user-path (build-path db-dir fname))))

;; Input
(define fnin.concepts "concepts.txt")
(define fnin.edges "paired_concept_counts_associations.txt")
(define header.concepts '("concept_id" "concept_name" "domain_id" "vocabulary_id" "concept_class_id" "concept_code"))
(define header.edges '("dataset_id" "concept_id_1" "concept_id_2" "concept_count" "concept_prevalence" "chi_square_t" "chi_square_p" "expected_count" "ln_ratio" "rel_freq_1" "rel_freq_2"))

(define (validate-header header-expected in)
  (define header-found (read-line in 'any))
  (when (not (equal? header-found (string-join header-expected "\t")))
    (error "unexpected header:" header-found header-expected)))

(define (materialize-dsv-stream in header transform . mat-args)
  (let ((mat (apply materializer mat-args)))
    (validate-header header in)
    (define count 0)
    (time (s-each (lambda (x)
                    (when (= 0 (remainder count 100000))
                      (printf "Ingested ~s rows\n" count))
                    (mat 'put x)
                    (set! count (+ count 1)))
                  (if transform (s-map transform (tsv->stream in))
                    (tsv->stream in))))
    (printf "Processing ~s rows\n" count)
    (time (mat 'close))
    (printf "Finished processing ~s rows\n" count)))

;; materialize a relation if not present
(define (materialize-relation name fnin header transform fields types indexes)
  (unless (directory-exists? (db-path name))
    (printf "buildling relation: ~s; ~s\n"
            (db-path fnin) (db-path name))
    (let/files ((in (db-path fnin))) ()
      (materialize-dsv-stream
        in header transform
        `((buffer-size     . ,buffer-size)    ; optional
          (path            . ,(db-path name))
          (attribute-names . ,fields)
          (attribute-types . ,types)
          (tables  ((columns . ,fields)))     ; optional if same order as attribute-names
          (indexes . ,(map (lambda (i) (list (cons 'columns i))) indexes)))))))

(materialize-relation
  "concept" fnin.concepts header.concepts
  (lambda (row)
    (match-define (list id name domain vocab class code) row)
    (list (string->number id) (string-append (string-upcase vocab) ":" code)
          name domain class))
  '(id curie name domain class)
  '(nat string string string string)
  '((curie)
    (name)
    (class)
    (domain)))

(materialize-relation
  "edge" fnin.edges header.edges
  (lambda (row)
    (define-values (left right) (split-at row 4))
    (append (map string->number left) right))
  '(dataset subject object concept_count prevalence chi_sq_t chi_sq_p expected_count ln_ratio rel_freq1 rel_freq2)
  '(nat nat nat nat string string string string string string string)
  '((subject object)
    (object)))

;; all indexes have implicit index made on first arg/no need to index separately

(time (let ()
        ;; ~4x faster retrieval; ~400x slower loading
        ;(define-materialized-relation concept   `((path . ,(db-path "concept")) (retrieval-type . bytes)))
        ;; ~10x faster retrieval; ~6000x slower loading
        ;(define-materialized-relation concept   `((path . ,(db-path "concept")) (retrieval-type . scm)))
        ;; baseline; including (retrieval-type . disk) is optional
        (define-materialized-relation concept `((path . ,(db-path "concept"))))
        (define-materialized-relation edge    `((path . ,(db-path "edge"))))
        (time (pretty-print
                (run 10 (id name domain class)
                  (concept id "RXNORM:763521" name domain class))))
        (newline)
        ;; make a new relation that reflects what you want from edge
        (define-relation (concise-edge dataset subject object chi_sq_p)
          (fresh (concept_count prevalence chi_sq_t expected_count ln_ratio rel_freq1 rel_freq2)
            (edge dataset subject object concept_count prevalence chi_sq_t chi_sq_p expected_count ln_ratio rel_freq1 rel_freq2)))
        (time (pretty-print
                (run 10 (id name dataset subject chi_sq_p)
                  (fresh (domain class)
                    (concept id "SNOMED:62208003" name domain class)
                    (concise-edge dataset subject id chi_sq_p)))))))
