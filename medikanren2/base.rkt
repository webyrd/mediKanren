#lang racket/base
(provide cfg:config cfg:config-ref cfg:load-config cfg:override-config
         (all-from-out "dbk/dbk.rkt") load-config
         relation-name relation-definition-info relation-missing-data?
         tagged-relation dynamic-relation relation-extensions database-extend-relations! database-load! database-unload!)
(require
  "dbk/dbk.rkt"
  racket/list (except-in racket/match ==) racket/runtime-path racket/set
  racket/dict
  (prefix-in cfg: "configref.rkt"))

(define-runtime-path path.root ".")
(define (path-simple path) (path->string (simplify-path path)))
(define (path/root relative-path)
  (path-simple (build-path path.root relative-path)))
(define path.data (path/root "data"))
(define (path/data relative-path)
  (path-simple (build-path path.data relative-path)))

(define (load-config (verbose? #t))
  (cfg:load-config verbose?)
  (cfg:override-config
    (list (cons 'relation-root-path  path.data)
          (cons 'temporary-root-path (path/data "temporary"))))
  ;; populate configuration of dbKanren
  (define config-for-dbkanren
    (map (lambda (kv)
      (define k (car kv))
      (define v (cfg:config-ref k))
      (cons k v))
      (dict->list dbk:config.default)))
  (dbk:current-config-set!/alist #;(dbk:current-config) config-for-dbkanren))

(load-config #t)

(define (relation-name            r) (hash-ref (relations-ref r)            'name))
(define (relation-definition-info r) (hash-ref (relations-ref r)            'definition-info))
(define (relation-missing-data?   r) (hash-ref (relation-definition-info r) 'missing-data? #f))

(define name.r=>tagged-relations   (hash))
(define name.db=>name.r=>relations (hash))

(define (database-extend-relations! name.db . extensions)
  (define nr*s (plist->alist extensions))
  (set! name.db=>name.r=>relations
    (hash-update name.db=>name.r=>relations name.db
                 (lambda (name=>relations)
                   (foldl (lambda (name relation n=>rs)
                            (hash-update n=>rs name
                                         (lambda (rs) (cons relation rs))
                                         '()))
                          name=>relations (map car nr*s) (map cdr nr*s)))
                 (hash))))

(define (database-load! name.db)
  (define name=>relations (hash-ref name.db=>name.r=>relations name.db
                                    (lambda () (error "unknown database:" name.db))))
  (define missing (filter-not not (append* (map (lambda (rs)
                                                  (map (lambda (r)
                                                         (and (relation-missing-data? r)
                                                              (relation-name          r)))
                                                       rs))
                                                (hash-values name=>relations)))))
  (unless (null? missing)
    (error "loaded database has relations that are missing data:" name.db missing))
  (define nr*s (hash->list name=>relations))
  (set! name.r=>tagged-relations
    (foldl (lambda (name relations n=>rs)
             (define tagged-relations (list->set (map (lambda (r) (cons name.db r)) relations)))
             (hash-update n=>rs name
                          (lambda (rs) (set-union rs tagged-relations))
                          (set)))
           name.r=>tagged-relations (map car nr*s) (map cdr nr*s))))

(define (database-unload! name.db)
  (define name=>relations (hash-ref name.db=>name.r=>relations name.db
                                    (lambda () (error "unknown database:" name.db))))
  (define nr*s (hash->list name=>relations))
  (set! name.r=>tagged-relations
    (foldl (lambda (name relations n=>rs)
             (define tagged-relations (list->set (map (lambda (r) (cons name.db r)) relations)))
             (hash-update n=>rs name
                          (lambda (rs) (set-subtract rs tagged-relations))
                          (set)))
           name.r=>tagged-relations (map car nr*s) (map cdr nr*s))))

(define (relation-extensions name)
  (set->list (hash-ref name.r=>tagged-relations name (set))))

(define ((tagged-relation r tag . tag-positions) . args)
  (let loop ((args args) (tag-positions tag-positions))
    (match tag-positions
      ('()                      (apply r args))
      ((cons pos tag-positions) (fresh (x)
                                  (== (list-ref args pos) (cons tag x))
                                  (loop (list-set args pos x)
                                        tag-positions))))))

(define (dynamic-relation name . tag-positions)
  (define extensions (relation-extensions name))
  (define tagged-relations
    (map (lambda (name.db r.db) (apply tagged-relation r.db name.db tag-positions))
         (map car extensions)
         (map cdr extensions)))
  (lambda args
    (foldl (lambda (r g)
             (conde ((apply r args))
                    (g)))
           (== #f #t)
           tagged-relations)))
