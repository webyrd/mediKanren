#lang racket/base
(provide
  (all-from-out "dbk/dbk.rkt")
  path/data path/root path.data path.root

  config config-ref load-config

  read/file read/string
  )
(require
  "dbk/dbk.rkt"
  racket/list racket/port racket/runtime-path)

(define-runtime-path path.root ".")
(define (path/root relative-path)
  (path->string (build-path path.root relative-path)))
(define path.data (path/root "data"))
(define (path/data relative-path)
  (path->string (build-path path.data relative-path)))

(define (path-simple path) (path->string (simplify-path path)))

(define (read/file path)  (with-input-from-file  path read))
(define (read/string str) (with-input-from-string str read))

(define box.config (box #f))
(define (config)
  (define cfg (unbox box.config))
  (cond (cfg cfg)
        (else (load-config #t #f)
              (unbox box.config))))
(define (config-ref key)
  (define kv (assoc key (config)))
  (unless kv (error "missing configuration key:" key))
  (cdr kv))
(define (load-config verbose? path.config)
  (define path.config.user     (or path.config (path/root "config.scm")))
  (define path.config.defaults (path/root "config.defaults.scm"))
  (when verbose? (printf "loading configuration defaults: ~a\n"
                         (path-simple path.config.defaults)))
  (when verbose? (printf "loading configuration overrides: ~a\n"
                         (path-simple path.config.user)))
  (define config.defaults (read/file path.config.defaults))
  (define config.user     (if (file-exists? path.config.user)
                            (read/file path.config.user)
                            '()))
  (unless (and (list? config.user) (andmap pair? config.user))
    (error "invalid configuration overrides:" config.user))
  (define user-keys (map car config.user))
  (define (user-defined? kv) (member (car kv) user-keys))
  (set-box! box.config
            (append config.user (filter-not user-defined? config.defaults))))
