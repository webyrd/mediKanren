#lang racket/base
(provide

  )

(require

  )

;; Goals:
;; raw mappings for SQL tables
;; foreign key cross-references
;; mk relations that resolve foreign keys
;; later: incorporate indices

;; parse SQL creation statements:
'((many* (or create-table create-index)))

'(((create-table table-name fields uniques foreign-keys)
   "CREATE" "TABLE" (bq table-name)
   ;; TODO: comma separation
   (paren (seq (and fields (seq (maybe field-primary-key)
                                (many* field)))
               (maybe primary-key)
               (and uniques (many* unique))
               (and foreign-keys (many* foreign-key))))
   ";")

  ((create-index index-name table-name field-name)
   "CREATE" "INDEX" (dq index-name) "ON"
   (dq table-name) (paren ((bq field-name)))
   ";")

  ((field-primary-key name ftype)
   (bq name) (field-type ftype) (many* _) "PRIMARY" "KEY" ignore-up-to-comma-or-paren)

  ((field name ftype)
   (bq name) (field-type ftype) ignore-up-to-comma-or-paren)

  ((field-type)
   (or "timestamp" "integer" (seq "decimal" (paren _)) "text" (seq "varchar" (paren _))))

  ((primary-key fname) "PRIMARY" "KEY" (paren (bq fname)))

  ((unique field-names) "UNIQUE" (paren field-names))

  ((foreign-key name local-field table foreign-field)
   "CONSTRAINT" name "FOREIGN" "KEY" (paren local-field)
   "REFERENCES" table (paren foreign-field) ignore-up-to-comma-or-paren)

  ;; TODO: comma-separated content
  ((paren content) "(" ,@content ")")

  ((dq name) "\"" name "\"")

  ((bq name) "`" name "`"))
