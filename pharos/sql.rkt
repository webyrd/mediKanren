#lang racket/base
(provide

  )

(require
  "mk.rkt"
  "mk-parse.rkt"
  racket/file
  )

;; Goals:
;; raw mappings for SQL tables
;; foreign key cross-references
;; mk relations that resolve foreign keys
;; later: incorporate indices

;; parse SQL creation statements:
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
  (or/p ((remember (one-of '("integer" "text" "timestamp" "date"))) type)
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
       (skip-until (append '("PRIMARY") delimiters))
       "PRIMARY" "KEY" (skip-until delimiters)))

(define (field/non-primary-key name type)
  (seq (bq name) (field-type type)
       (skip* (none-of (append '("PRIMARY" "KEY") delimiters)))
       (forget delimiter)))

(define (primary-key names)
  (seq "PRIMARY" "KEY" (paren bq names) (forget delimiter)))

(define (unique names) (seq "UNIQUE" (paren bq names) (forget delimiter)))

(define (foreign-key fk-name local-names table foreign-names)
  (seq "CONSTRAINT" (bq fk-name) "FOREIGN" "KEY" (paren bq local-names)
       "REFERENCES" (bq table) (paren bq foreign-names)
       (skip-until delimiters)))

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

;; Tokenization
(define chars-ws '(#\space #\tab #\newline #\return #\vtab #\page))
(define chars-hws '(#\space #\tab))

(define chars-quote '(#\' #\" #\`))
(define chars-bracket '(#\( #\) #\[ #\] #\{ #\}))
(define chars-separator '(#\, #\; #\: #\.))
(define chars-symbolic (append chars-quote chars-bracket chars-separator))

(define skip-ws (skip-while chars-ws))
(define skip-hws (skip-while chars-hws))

(define (token* ts)
  (define chars-ws&symbolic (append chars-symbolic chars-ws))
  (define (token t)
    (seq skip-ws
         (or/p (fresh/p (c)
                 (== (list c) t)
                 ((remember (one-of chars-symbolic)) c))
               ((many+-until chars-ws&symbolic) t))))
  (seq ((many* token) ts) skip-ws end))

(define (tokenize str)
  (map list->string (car (run* (ts) ((token* ts) (string->list str) '())))))

;; First attempt at token*, factoring out split-ws*.  It's a lot more involved.
;(define (split-ws* blocks)
  ;(define (non-ws* block)
    ;(seq skip-ws
         ;((many+ (remember (none-of chars-ws))) block)
         ;(or/p end (forget (one-of chars-ws)))))
  ;(seq ((many* non-ws*) blocks) skip-ws end))

;(define (split-symbolic* bs)
  ;(define (block s)
    ;(or/p (lambda (in out)
            ;(fresh (c)
              ;(== (list c) s)
              ;(((remember (one-of chars-symbolic)) c) in out)))
          ;((many+-until chars-symbolic) s)))
  ;(seq ((many* block) bs) end))

;(define (appendo l s ls)
  ;(conde
    ;((== '() l) (== s ls))
    ;((fresh (a d ms)
       ;(== `(,a . ,d) l)
       ;(== `(,a . ,ms) ls)
       ;(appendo d s ms)))))

;(define (flatteno xss flattened)
  ;(conde
    ;((== '() xss) (== '() flattened))
    ;((fresh (xs yss yflattened)
       ;(== `(,xs . ,yss) xss)
       ;(flatteno yss yflattened)
       ;(appendo xs yflattened flattened)))))

;(define (mapo R xs zs)
  ;(conde
    ;((== '() xs) (== '() zs))
    ;((fresh (a d y ys)
       ;(== `(,a . ,d) xs)
       ;(== `(,y . ,ys) zs)
       ;(R a y)
       ;(mapo R d ys)))))

;(define (token* ts)
  ;(define (parse in bs) ((split-symbolic* bs) in '()))
  ;(lambda (in out)
    ;(fresh (bs tss)
      ;((split-ws* bs) in out)
      ;(mapo parse bs tss)
      ;(flatteno tss ts))))
