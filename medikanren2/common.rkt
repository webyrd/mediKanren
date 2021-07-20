#lang racket/base
(provide (all-from-out "base.rkt"
                       "db/semmed.rkt"
                       "db/rtx2-20210204.rkt"
                       "db/kgx-synonym.rkt") ; workaround:
                                             ; so long as KGX_synonym isn't specified in config.scm
                                             ; this won't signal an error even if KGX isn't present
         cprop edge eprop
         triple quad triple/eid is-a is-a/quad triple-property
         write-list-to-tsv)

(require "base.rkt"
         "string-search.rkt"
         (except-in racket/match ==)
         (prefix-in semmed: "db/semmed.rkt")
         (prefix-in rtx:    "db/rtx2-20210204.rkt")
         (prefix-in kgx:    "db/kgx-synonym.rkt"))

(for-each database-load! (hash-ref (current-config) 'databases))

;; TODO: define higher-level relations over the db-specific relations

(define cprop (dynamic-relation 'cprop))
;; tag argument 0 (the edge id) with database name
(define edge  (dynamic-relation 'edge  0))
(define eprop (dynamic-relation 'eprop 0))

;; Semantic-web flavored relations

(define-relation (rtx:triple s p o)
    (fresh (id)
      (rtx:eprop id "predicate" p)
      (rtx:edge id s o)))

(define-relation (semmed:triple s p o)
    (fresh (id)
      (semmed:eprop id "edge_label" p)
      (semmed:edge id s o)))

(define-relation (triple s p o)
  (conde ((rtx:triple s p o))
         ((semmed:triple s p o))))

(define-relation (quad graph s p o)
  (fresh (id)
    (conde ((== graph 'rtx2-20210204)
            (rtx:triple s p o))
           ((== graph 'semmed)
            (semmed:triple s p o)))))

(define-relation (triple/eid eid s p o)
  (fresh (id graph)
    (== eid `(,graph . ,id))
    (conde ((== graph 'rtx2-20210204)
            (rtx:eprop id "predicate" p)
            (rtx:edge id s o))
           ((== graph 'semmed)
            (semmed:eprop id "edge_label" p)
            (semmed:edge id s o)))))

(define-relation (is-a s c)
  (cprop s "category" c))

(define-relation (is-a/quad graph s c)
  (conde ((== graph 'rtx2-20210204)
          (rtx:cprop s "category" c))
         ((== graph 'semmed)
          (semmed:cprop s "category" c))))

(define-relation (triple-property s p o k v)
  (fresh (eid id graph)
    (== eid `(,graph . ,id))
    (conde ((== graph 'rtx2-20210204)
            (rtx:eprop id "predicate" p)
            (rtx:edge id s o)
            (rtx:eprop id k v))
           ((== graph 'semmed)
            (semmed:eprop id "edge_label" p)
            (semmed:edge id s o)
            (semmed:eprop id k v)))))

(define-relation (edge-predicate eid p)
  (eprop eid "predicate" p))

;; usage:
;(run*/set/steps 500 x (syn* "HGNC:5993" x))

(define write-list-to-tsv
  (lambda (header-ls lol path)
    (with-output-to-file path
      ;; thunk -- procedure that takes no arguments
      (lambda ()
        (for-each
          (lambda (l)
            (let loop ([l l])
              (cond
                ((null? l)
                 (error 'output-to-tsv "where's the data!?"))
                ((null? (cdr l)) ;; l contains exactly 1 element
                 (display (car l))
                 (display #\newline))
                (else
                 (display (car l))
                 (display #\tab)
                 (loop (cdr l))))))
          (cons header-ls lol)))
      #:mode 'text
      #:exists 'replace)))

; *** Workaround for:
;   loading configuration overrides: .../medikanren2/config.scm
;   checking for index .../medikanren2/data/semmed/cprop/concept-name-index.bytes
;   open-input-file: cannot open input file
;
; If we initialize dbs when they are required, then there is no opportunity
; to consult the 'databases configuration key.  For example, if semmed is not
; installed and not configured, requiring common.rkt will create the error above.
;
; By consulting 'databases here, we avoid the crash.  However, we lose the ability
; for each database to be specific about when/how to index each relation.  For now,
; we hard-code indexing on 'cprop.
;
; TODO: Allow databases to choose whether or not to configure string search
; on a particular relation, without breaking common.rkt.
(let ((databases (hash-ref (current-config) 'databases)))
  (for ((name.rel (relation-extensions 'cprop)))
    (match name.rel
      (`(,name . ,rel)
        (when (member name databases)
          (printf "about to init-rel name=~a\n" name)
          (string-search-init-rel rel))))))

