#lang racket/base
(provide (all-from-out "dbk/dbk.rkt") load-config)
(require
  "dbk/dbk.rkt"
  racket/list racket/port racket/runtime-path racket/string)

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
