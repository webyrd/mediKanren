#lang racket/base
(provide

  )

(require
  "mk.rkt"
  "mk-parse.rkt"
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

  ((primary-key fname)
   (fresh (bqfname)
     (seq "PRIMARY" "KEY" (paren (list bqfname)))
     (parse (bq fname) bqfname "")))

  ((unique field-names) "UNIQUE" (paren field-names))

  ((foreign-key name local-field table foreign-field)
   (seq "CONSTRAINT" (bq name) "FOREIGN" "KEY" (paren (list local-field))
        "REFERENCES" (bq table) (paren (list foreign-field)) ignore-up-to-comma-or-paren))

  ((paren content) (seq "(" (comma-separated content) ")"))

  ((comma-separated items)
   (fresh (first rest)
     (== `(,first . ,rest) items)
     (seq (alphanumeric first) (many* (seq "," (comma-separated rest))))))

  ((dq name) (seq "\"" (alphanumeric name) "\""))

  ((bq name) (seq "`" (alphanumeric name) "`")))


;; alphanumeric
;; numeric
;; skip until comma/paren

;; tokenizer:
;; parens
;; comma
;; semicolon
;; singlequote
;; doublequote
;; backquote

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
         (or/p (lambda (in out)
                 (fresh (c)
                   (== (list c) t)
                   (((remember (one-of chars-symbolic)) c) in out)))
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
