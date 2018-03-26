#lang racket/base
(provide

  )

(require
  "csv.rkt"
  "repr.rkt"
  racket/file
  racket/list
  racket/port
  racket/set
  racket/string
  racket/system
  )

;; TODO: Process SQL schema

;; Goals:
;; raw mappings for SQL tables
;; foreign key cross-references
;; mk relations that resolve foreign keys
;; later: incorporate indices

;; indexed fields may or may not be unique
;; unique: hash => row (no extra sorting necessary)
;; non-unique: hash => sorted row buckets

(define set-empty (set))
(define hash-empty (hash))

(define (subsumed? rs s) (ormap (lambda (r) (subset? r s)) rs))
(define (unique-add/subsume unique uniques)
  (define us (set->list uniques))
  (if (subsumed? us unique) uniques
    (set-add (list->set (filter (lambda (u) (not (subset? unique u))) us))
             unique)))

(define (schema-table->table t)
  (define name (car t))
  (define fields (cdr (assoc 'field (cdr t))))
  (define foreigns (map cdr (cdr (assoc 'foreign (cdr t)))))
  (define raw-uniques (map list->set (cdr (assoc 'unique (cdr t)))))
  (define uniques (list->set (foldl unique-add/subsume '() raw-uniques)))
  (table-new name fields foreigns uniques set-empty))

(define (table-new name fields foreigns uniques sortings)
  `(,name ,fields ,foreigns ,uniques ,sortings))
(define (table-name t) (car t))
(define (table-fields t) (cadr t))
(define (table-foreigns t) (caddr t))
(define (table-uniques t) (cadddr t))
(define (table-sortings t) (cadddr (cdr t)))
(define (table-uniques-add t unique)
  (if (set-member? (table-uniques t) unique) t
    (table-new (table-name t) (table-fields t) (table-foreigns t)
               (unique-add/subsume unique (table-uniques t))
               (set-remove (table-sortings t) unique))))
(define (table-sortings-add t sorting)
  (if (or (set-member? (table-sortings t) sorting)
          (set-member? (table-uniques t) (list->set sorting))) t
    (table-new (table-name t) (table-fields t) (table-foreigns t)
               (table-uniques t) (set-add (table-sortings t) sorting))))

(define (schema->tables schema)
  (define tables (cdr (assoc 'tables schema)))
  (define indexes (cdr (assoc 'indexes schema)))
  (define name=>table
    (foldl (lambda (s name=>table)
             (define t (schema-table->table s))
             (hash-set name=>table (table-name t) t))
           hash-empty tables))
  (foldl (lambda (index name=>table)
           (define tname (cadr index))
           (define tfields (caddr index))
           (define table (hash-ref name=>table tname))
           (hash-set name=>table tname (table-sortings-add table tfields)))
         name=>table indexes))

(define (table->dump* t)
  (define field-names (map car (table-fields t)))
  (define (select ordering)
    (string-join (list "SELECT" (string-join field-names ",")
                       "FROM" (table-name t)
                       ;; TODO: remove LIMIT after testing.
                       ordering) " "))
  (define (sorting->ordering sorting)
    (string-append
      "ORDER BY "
      (string-join (map (lambda (col) (string-join (list col "ASC") " "))
                        sorting)
                   ", ")))
  (define sortings (append (map set->list (set->list (table-uniques t)))
                           (set->list (table-sortings t))))
  (define orderings (map sorting->ordering sortings))
  (map list
       (if (null? sortings) (list #f) sortings)
       (map select (if (null? orderings) (list "") orderings))))

(define (string->datum type str)
  (cond ((string=? "integer" type) (integer->repr (string->number str)))
        ((string=? "decimal" type) (decimal->repr (string->number str)))
        ;((or (string=? "text" type)
             ;(string=? "timestamp" type)
             ;(string=? "date" type)) (string->repr str))
        ;(else (error "invalid field type:" type str))
        (else (string->repr str))))

;; TODO: replace with a more compact format.
(define (csv->scm table-name field-types detail-out offset-out)
  (lambda (in)
    (define (record->datum record)
      (when (not (= (length field-types) (length record)))
        (printf "~s\n" `(record->datum
                          ,table-name ,(length field-types) ,(length record)
                          ,field-types ,record)))
      (data->repr (map string->datum field-types record)))
    (define (yield-record! record)
      (detail-write detail-out offset-out (record->datum record)))
    ((csv-records yield-record!) in)
    (flush-output detail-out)
    (flush-output offset-out)))

(define (run-sql consume sql)
  (define proc
    (process
      (string-append
        "sqlite3 -bail -csv " (path->string sqlite-db-path) " '" sql "'")))
  (define pout (car proc))
  (define pin (cadr proc))
  (define perr (cadddr proc))
  (close-output-port pin)
  (consume pout)
  (close-input-port pout)
  (close-input-port perr))

;(define argv '#("schema.scm" "data"))
(define argv (current-command-line-arguments))
(define argv-expected '#(SCHEMA_FILE DATA_DIR))
(when (not (= (vector-length argv-expected) (vector-length argv)))
  (error (format "cmd-line-args expected ~s; given ~s" argv-expected argv)))

(define schema-path (expand-user-path (vector-ref argv 0)))
(define data-dir (expand-user-path (vector-ref argv 1)))
(define sqlite-db-path (build-path data-dir "pharos-sqlite3.db"))
(define db-dir (build-path data-dir "db"))
(define tables-dir (build-path db-dir "tables"))

(define name=>table-info
  (make-immutable-hash
    (map (lambda (t) (cons (table-name t) (list t (table->dump* t))))
         (hash-values
           (schema->tables (call-with-input-file schema-path read))))))

(for (((name tinfo) (in-hash name=>table-info)))
     (define t (car tinfo))
     (define field-types (map cadr (table-fields t)))
     (define indexes (cadr tinfo))
     (define tpath (build-path tables-dir name))
     (define cpath (build-path tpath "columns.scm"))
     (define ipath (build-path tpath "indexes.scm"))
     (define fkpath (build-path tpath "foreign-keys.scm"))
     (make-directory* (expand-user-path tpath))
     (call-with-output-file cpath (lambda (out) (write (table-fields t) out)))
     (call-with-output-file
       ipath (lambda (out) (write (map car indexes) out)))
     (call-with-output-file
       fkpath (lambda (out) (write (table-foreigns t) out)))
     (for ((ix indexes) (i (range (length indexes))))
          (define sorting-dir (build-path tpath (number->string i)))
          (define detail-path (build-path sorting-dir "detail.scm"))
          (define offset-path (build-path sorting-dir "offset.bin"))
          (make-directory* (expand-user-path sorting-dir))
          (call-with-output-file
            detail-path
            (lambda (detail-out)
              (call-with-output-file
                offset-path
                (lambda (offset-out)
                  (run-sql (csv->scm name field-types detail-out offset-out)
                           (cadr ix))))))))
