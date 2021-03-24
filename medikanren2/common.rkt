#lang racket/base
(provide (all-from-out "base.rkt"
                       "db/semmed.rkt"
                       "db/rtx2-20210204.rkt")
         cprop edge eprop)
(require "base.rkt"
         (prefix-in semmed: "db/semmed.rkt")
         (prefix-in rtx:    "db/rtx2-20210204.rkt")
         racket/list (except-in racket/match ==) racket/pretty)

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

(define (cprop c k v)
  (conde ((semmed:cprop c k v))
         ((rtx:cprop    c k v))))

(define (edge eid s o)
  (conde ((fresh (id) (== eid `(semmed        . ,id)) (semmed:edge id s o)))
         ((fresh (id) (== eid `(rtx2-20210204 . ,id)) (rtx:edge    id s o)))))

(define (eprop eid k v)
  (conde ((fresh (id) (== eid `(semmed        . ,id)) (semmed:eprop id k v)))
         ((fresh (id) (== eid `(rtx2-20210204 . ,id)) (rtx:eprop    id k v)))))
