#lang racket/base
(provide (all-from-out "dbk/dbk.rkt") load-config
         relation-name relation-definition-info relation-missing-data?
         tagged-relation dynamic-relation relation-extensions database-extend-relations! database-load! database-unload!)
(require
  "dbk/dbk.rkt"
  racket/list (except-in racket/match ==) racket/runtime-path racket/set)

(define-runtime-path path.root ".")
(define (path-simple path) (path->string (simplify-path path)))
(define (path/root relative-path)
  (path-simple (build-path path.root relative-path)))
(define path.data (path/root "data"))
(define (path/data relative-path)
  (path-simple (build-path path.data relative-path)))

(define path.config.defaults (path-simple (path/root "config.defaults.scm")))
(define path.config.override (path-simple (path/root "config.scm")))

(define (load-config (verbose? #t) (path.config #f))
  (define (config/file path)
    (config-set/alist
      (current-config)
      (append (list (cons 'relation-root-path  path.data)
                    (cons 'temporary-root-path (path/data "temporary")))
              (with-input-from-file path read))))
  (when verbose? (eprintf "loading configuration defaults: ~a\n"
                         path.config.defaults))
  (current-config (config/file path.config.defaults))
  (define path (or (and path.config (file-exists? path.config) path.config)
                   (and (file-exists? path.config.override)
                        path.config.override)))
  (when path
    (when verbose? (eprintf "loading configuration overrides: ~a\n" path))
    (current-config (config/file path))))

(load-config)

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
