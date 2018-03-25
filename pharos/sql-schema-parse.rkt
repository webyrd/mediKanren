#lang racket/base
(require
  "mk.rkt"
  "mk-parse.rkt"
  )

;; Parse SQL schema
(define delimiters '("," ")") )
(define delimiter (one-of delimiters))

(define (sq name) (seq "\'" (single name) "\'"))
(define (dq name) (seq "\"" (single name) "\""))
(define (bq name) (seq "`" (single name) "`"))

(define (paren p items)
  (define (comma-prefixed item) (seq "," (p item)))
  (fresh/p (first rest)
    (== `(,first . ,rest) items)
    (seq "(" (p first) ((many* comma-prefixed) rest) ")")))

(define (field-type type)
  (or/p ((remember1 (one-of '("integer" "text" "timestamp" "date"))) type)
        (fresh/p (m n)
          (== "decimal" type)
          (seq "decimal" "(" (single m) "," (single n) ")"))
        (fresh/p (_)
          (== "text" type)
          (seq "varchar" "(" (single _) ")"))
        (fresh/p (_)
          (== "text" type)
          (seq "char" "(" (single _) ")"))))

(define (field/primary-key name type)
  (seq (bq name) (field-type type)
       (skip*-until (append '("PRIMARY") delimiters))
       "PRIMARY" "KEY" (skip*-until delimiters)))

(define (field/non-primary-key name type)
  (seq (bq name) (field-type type)
       (skip* (none-of (append '("PRIMARY" "KEY") delimiters)))
       (peek delimiter)))

(define (primary-key names)
  (seq "PRIMARY" "KEY" (paren bq names) (peek delimiter)))

(define (unique names) (seq "UNIQUE" (paren bq names) (peek delimiter)))

(define (foreign-key fk-name local-names table foreign-names)
  (seq "CONSTRAINT" (bq fk-name) "FOREIGN" "KEY" (paren bq local-names)
       "REFERENCES" (bq table) (paren bq foreign-names)
       (skip*-until delimiters)))

(define (create-table table-name body*)
  (define (field-or-cx datum)
    (or/p (fresh/p (name type)
            (== `(field/primary-key ,name ,type) datum)
            (field/primary-key name type))
          (fresh/p (name type)
            (== `(field/non-primary-key ,name ,type) datum)
            (field/non-primary-key name type))
          (fresh/p (names)
            (== `(primary-key ,names) datum)
            (primary-key names))
          (fresh/p (names)
            (== `(unique ,names) datum)
            (unique names))
          (fresh/p (fk-name locals table foreigns)
            (== `(foreign-key ,fk-name ,locals ,table ,foreigns) datum)
            (foreign-key fk-name locals table foreigns))))
  (seq "CREATE" "TABLE" (bq table-name) (paren field-or-cx body*) ";"))

(define (create-index index-name table-name field-names)
  (seq "CREATE" "INDEX" (dq index-name) "ON"
       (dq table-name) (paren bq field-names) ";"))

(define (schema body*)
  (define (table-or-index datum)
    (or/p (fresh/p (name tbody)
            (== `(table ,name ,tbody) datum)
            (create-table name tbody))
          (fresh/p (name tname fnames)
            (== `(index ,name ,tname ,fnames) datum)
            (create-index name tname fnames))))
  (seq ((many* table-or-index) body*) end))

(define (tokens->schema tokens)
  (define results (run* (body*) ((schema body*) tokens '())))
  (when (= 0 (length results)) (error "failed to parse"))
  (when (< 1 (length results))
    (error "ambiguous parse: count=" (length results)))
  (car results))

(write (tokens->schema (read)))
