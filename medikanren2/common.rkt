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
         (prefix-in semmed: "db/semmed.rkt")
         (prefix-in rtx:    "db/rtx2-20210204.rkt")
         (prefix-in kgx:    "db/kgx-synonym.rkt"))

(printf "Configuration says to load these databases: ~s\n" (cfg:config-ref 'databases))
(for-each database-load! (cfg:config-ref 'databases))

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
    (conde ((== graph rtx:kgid)
            (rtx:triple s p o))
           ((== graph 'semmed)
            (semmed:triple s p o)))))

(define-relation (triple/eid eid s p o)
  (fresh (id graph)
    (== eid `(,graph . ,id))
    (conde ((== graph rtx:kgid)
            (rtx:eprop id "predicate" p)
            (rtx:edge id s o))
           ((== graph 'semmed)
            (semmed:eprop id "edge_label" p)
            (semmed:edge id s o)))))

(define-relation (is-a s c)
  (cprop s "category" c))

(define-relation (is-a/quad graph s c)
  (conde ((== graph rtx:kgid)
          (rtx:cprop s "category" c))
         ((== graph 'semmed)
          (semmed:cprop s "category" c))))

(define-relation (triple-property s p o k v)
  (fresh (eid id graph)
    (== eid `(,graph . ,id))
    (conde ((== graph rtx:kgid)
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
