#lang racket/base
(provide (all-from-out "base.rkt"
                       "db/semmed.rkt"
                       "db/rtx2-20210204.rkt"
                       "db/KGX_synonym.rkt")
         cprop edge eprop
         triple quad triple/eid is-a is-a/quad triple-property
         synonym syn
         get-synonyms get-synonyms-ls
         write-list-to-tsv)

(require "base.rkt"
         (prefix-in semmed: "db/semmed.rkt")
         (prefix-in rtx:    "db/rtx2-20210204.rkt")
         "db/KGX_synonym.rkt"
         racket/list racket/set racket/string (except-in racket/match ==) racket/pretty)

(define dbname=>tabled-relations
  (hash 'semmed        semmed:tabled-relations
        'rtx2-20210204 rtx:tabled-relations))

(define missing
  (filter-not
    (lambda (dbname&missing)
      (match-define (list dbname missing) dbname&missing)
      (null? missing))
    (map (lambda (dbname)
           (list dbname
                 (filter-not
                   not
                   (map (lambda (r) (and (relation-missing-data? r)
                                         (relation-name r)))
                        (hash-ref dbname=>tabled-relations dbname)))))
         (hash-ref (current-config) 'databases))))

(unless (null? missing)
  (pretty-write `((configured-databases: . ,(hash-ref (current-config) 'databases))
                  (databases-with-empty-relations: . ,missing))
                (current-error-port))
  (error "missing data detected in configured databases:"
         (map car missing)))

;; TODO: define higher-level relations over the db-specific relations

(define-relation (cprop c k v)
  (conde ((rtx:cprop    c k v))
         ((semmed:cprop c k v))))

(define-relation (edge eid s o)
  (conde ((fresh (id) (== eid `(rtx2-20210204 . ,id)) (rtx:edge    id s o)))
         ((fresh (id) (== eid `(semmed        . ,id)) (semmed:edge id s o)))))

(define-relation (eprop eid k v)
  (conde ((fresh (id) (== eid `(rtx2-20210204 . ,id)) (rtx:eprop    id k v)))
         ((fresh (id) (== eid `(semmed        . ,id)) (semmed:eprop id k v)))))

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


(define synonyms-preds '("biolink:same_as"  ; rtx
                        ;"biolink:close_match" ; rtx
                         "biolink:has_gene_product" ;rtx
                         ))

(define-relation (direct-synonym a b)
  (fresh (id sp)
    (edge id a b)
    (eprop id "predicate" sp)
    (membero sp synonyms-preds)))

(define-relation (close-match-syn a b)
  (fresh (id)
    (any<=o "HGNC:" a)
    (any<=o a "HGND")
    (any<=o "UMLS:" b)
    (any<=o b "UMLT")
    (edge id a b)
    (:== #t (a) (not (null? (run 1 () (eprop id "predicate" "biolink:close_match")))))
    (:== #t (a) (string-prefix? a "HGNC:"))
    (:== #t (b) (string-prefix? b "UMLS:"))))

(define-relation (synonym a b)
  (conde ((== a b))
         ((close-match-syn a b))
         ((close-match-syn b a))
         ((direct-synonym a b))
         ((direct-synonym b a))
         ((fresh (mid)
            (close-match-syn a mid)
            (synonym mid b)))
         ((fresh (mid)
            (close-match-syn b mid)
            (synonym mid a)))         
         ((fresh (mid)
            (direct-synonym a mid)
            (synonym mid b)))
         ((fresh (mid)
            (direct-synonym b mid)
            (synonym mid a)))))

; define a get-synonyms function based on the synonym relation
; ideally we wanted to start with HGNC prefixes right now because if we start with other curie prefixes
; such as UMLS, we need to increase the steps
(define (get-synonyms a-curie)
  (set->list (run*/set/steps 500 s (synonym a-curie s))))

; get-synonyms-ls function takes a list of curies and return a list of all normalized curies
(define (get-synonyms-ls curie-ls)
  (define (unwrap lst)
    (cond
      [(null? lst) '()]
      [(pair? lst)
       (append (unwrap (car lst)) (unwrap (cdr lst)))]
      [else (list lst)]))
  (time (unwrap (map (lambda (x) (get-synonyms x)) curie-ls))))

; get-names-ls function takes a list of curies and return the curies with their coresponding names
(define (get-names-ls curie-ls)
  (run* (curie name)
    (cprop curie "name" name)
    (membero curie curie-ls)))


; The 2nd-approach to find synonyms is to use the cached synonyms from the KGX-syn KG
(define (syn a b)
  (fresh (predicate id source_database)
    (conde
      [(KGX-syn a b predicate id source_database)]
      [(KGX-syn b a predicate id source_database)])))

(define (syn* a b)
  (conde
    [(== a b)]
    [(syn a b)]
    [(fresh (mid)
       (syn a mid)
       (syn* mid b))]))

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
