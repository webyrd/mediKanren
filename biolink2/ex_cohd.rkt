#lang racket/base
(require "dbk/dbk.rkt" racket/function racket/pretty racket/string racket/list)


;; racket ex_cohd.rkt data cohd-v2 tsv
(define buffer-size 100000)  ;; main memory used for external sorting

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
(define fnin.concepts "concepts.txt")
(define fnin.edges "paired_concept_counts_associations.txt")
(define header.concepts '("concept_id" "concept_name" "domain_id" "vocabulary_id" "concept_class_id" "concept_code"))
(define header.edges '("dataset_id" "concept_id_1" "concept_id_2" "concept_count" "concept_prevalence" "chi_square_t" "chi_square_p" "expected_count" "ln_ratio" "rel_freq_1" "rel_freq_2"))


(define (validate-header header-expected in)
  (define header-found (read-line in 'any))
  (when (not (equal? header-found (string-join header-expected
                                               header-delimiter)))
    (error "unexpected header:" header-found header-expected)))

(define (materialize-dsv-stream in header . mat-args)
  (let ((mat (apply materializer mat-args)))
    (validate-header header in)
    (define count 0)
    (time (s-each (dsv->stream in)
                  (lambda (x)
                    (when (= 0 (remainder count 100000))
                      (printf "Ingested ~s rows\n" count))
                    (mat 'put x)
                    (set! count (+ count 1)))))
    (printf "Processing ~s rows\n" count)
    (time (mat 'close))
    (printf "Finished processing ~s rows\n" count)))



;; materialize a relation if not present
(define (materialize-relation name fnin header fields types indexes)
  (unless (directory-exists? (db-path name))
    (printf "buildling relation: ~s; ~s\n"
            (db-path fnin) (db-path name))
    (let/files ((in (db-path fnin))) ()
      (materialize-dsv-stream
        in header
        `((buffer-size     . ,buffer-size)    ; optional
          (path            . ,(db-path name))
          (source-columns  . ,fields)         ; optional given attribute-names
          (attribute-names . ,fields)
          (attribute-types . ,types)
          (tables  ((columns . ,fields)))     ; optional if same order as attribute-names
          (indexes . ,(map (lambda (i) (list (cons 'columns i))) indexes)))))))

(materialize-relation "concept" fnin.concepts header.concepts
                      '(id name domain vocab class code)                      
                      '(string string string string string string)
                      '((vocab code)
                        (name)
                        (class)
                        (domain)))


(let ((name "concept")
      (fnin fnin.concepts)
      (header header.concepts)
      (fields '(id name domain vocab class code))
      (types '(nat string string string string string))
      (indexes '((vocab code)
                 (name)
                 (class)
                 (domain))))
  (unless (directory-exists? (db-path name))
    (printf "buildling relation: ~s; ~s\n"
            (db-path fnin) (db-path name))
    (let/files ((in (db-path fnin))) ()
               (let ((in in)
                     (header header)
                     (mat-args `((buffer-size     . ,buffer-size) ; optional
                                 (path            . ,(db-path name))
                                 (source-columns  . ,fields) ; optional given attribute-names
                                 (attribute-names . ,fields)
                                 (attribute-types . ,types)
                                 (tables  ((columns . ,fields))) ; optional if same order as attribute-names
                                 (indexes . ,(map (lambda (i) (list (cons 'columns i))) indexes)))))
                 (let ((mat (materializer mat-args)))
                   (validate-header header in)
                   (define count 0)
                   (time (s-each (s-map (lambda (row)
                                          (cons (string->number (car row)) (cdr row)))
                                        (dsv->stream in))
                                 (lambda (x)
                                   (when (= 0 (remainder count 100000))
                                     (printf "Ingested ~s rows\n" count))
                                   (mat 'put x)
                                   (set! count (+ count 1)))))
                   (printf "Processing ~s rows\n" count)
                   (time (mat 'close))
                   (printf "Finished processing ~s rows\n" count))))))




(let ((name "edges")
      (fnin fnin.edges)
      (header header.edges)
      (fields '(dataset subject object concept_count prevalence chi_sq_t chi_sq_p expected_count ln_ratio rel_freq1 rel_freq2))
      (types '(nat nat nat nat string string string string string string string))
      (indexes '((subject object)
                 (object))))
  (unless (directory-exists? (db-path name))
    (printf "buildling relation: ~s; ~s\n"
            (db-path fnin) (db-path name))
    (let/files ((in (db-path fnin))) ()
               (let ((in in)
                     (header header)
                     (mat-args `((buffer-size     . ,buffer-size) ; optional
                                 (path            . ,(db-path name))
                                 (source-columns  . ,fields) ; optional given attribute-names
                                 (attribute-names . ,fields)
                                 (attribute-types . ,types)
                                 (tables  ((columns . ,fields))) ; optional if same order as attribute-names
                                 (indexes . ,(map (lambda (i) (list (cons 'columns i))) indexes)))))
                 (let ((mat (materializer mat-args)))
                   (validate-header header in)
                   (define count 0)
                   (time (s-each (s-map (lambda (row)
                                          (define-values (left right) (split-at row 4))
                                          (append (map string->number left) right))
                                        (dsv->stream in))
                                 (lambda (x)
                                   (when (= 0 (remainder count 100000))
                                     (printf "Ingested ~s rows\n" count))
                                   (mat 'put x)
                                   (set! count (+ count 1)))))
                   (printf "Processing ~s rows\n" count)
                   (time (mat 'close))
                   (printf "Finished processing ~s rows\n" count))))))


;; all indexes have implicit index made on first arg/no need to index separately

 


(time (let ()
        ;; ~4x faster retrieval; ~400x slower loading
        ;(define-materialized-relation concept   `((path . ,(db-path "concept")) (retrieval-type . bytes)))
        ;; ~10x faster retrieval; ~6000x slower loading
        ;(define-materialized-relation concept   `((path . ,(db-path "concept")) (retrieval-type . scm)))
        ;; baseline; including (retrieval-type . disk) is optional
        (define-materialized-relation concept   `((path . ,(db-path "concept"))))
        (time (pretty-print
               (run 10 (id name domain vocab class code)
                 (concept id name domain "RxNorm" class "763521")
                 (concept id name domain vocab class code))))        
        (newline)
        ;; make a new relation that reflects what you want from edge
        (define-relation (concise-edge dataset subject object chi_sq_p)
          (fresh (edge concept_count prevalence chi_sq_t expected_count ln_ratio rel_freq1 rel_freq2)
              (edge dataset subject object concept_count prevalence chi_sq_t chi_sq_p expected_count ln_ratio rel_freq1 rel_freq2)))
        (time (pretty-print
               (run 10 (id name dataset subject chi_sq_p)
                 (fresh (domain class)
                   (concept id name domain "SNOMED" class "62208003")
                   (concise-edge dataset subject id chi_sq_p)))))))
