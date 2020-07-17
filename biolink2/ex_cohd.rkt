#lang racket/base
(require "common.rkt"
         (except-in racket/match ==)
         racket/list racket/pretty)

;; Command line usage: racket ex_cohd.rkt cohd-v2

(define argv (current-command-line-arguments))
(define argv-expected '#(DB_NAME))
(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error "command line argument mismatch:" argv-expected argv))

(define db-name  (vector-ref argv 0))
(define (db-relative-path path) (path->string (build-path db-name path)))
(define (db-path path) (path/data (db-relative-path path)))

;; Input
(define fnin.concepts "concepts.txt")
(define fnin.edges "paired_concept_counts_associations.txt")
(define header.concepts '("concept_id" "concept_name" "domain_id" "vocabulary_id" "concept_class_id" "concept_code"))
(define header.edges '("dataset_id" "concept_id_1" "concept_id_2" "concept_count" "concept_prevalence" "chi_square_t" "chi_square_p" "expected_count" "ln_ratio" "rel_freq_1" "rel_freq_2"))

(define (materialize-db-relation
          name fnin header transform fields types indexes)
  (materialize-relation
    (db-relative-path name) (db-relative-path fnin) header "\t"
    dsv->stream transform fields types indexes))

(materialize-db-relation
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

(materialize-db-relation
  "edge" fnin.edges header.edges
  (lambda (row)
    (define-values (left right) (split-at row 4))
    (append (map string->number left) right))
  '(dataset subject object concept_count prevalence chi_sq_t chi_sq_p expected_count ln_ratio rel_freq1 rel_freq2)
  '(nat nat nat nat string string string string string string string)
  '((subject)  ;; (subject object) doesn't work well currently
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
