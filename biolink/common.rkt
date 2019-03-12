#lang racket/base
(provide
  config
  config-ref
  path:root)
(require
  "mk-db.rkt"
  racket/list
  racket/runtime-path)

(define-runtime-path path:root ".")
(define (path/root relative-path) (build-path path:root relative-path))

(define path:config.user     (path/root "config.scm"))
(define path:config.defaults (path/root "config.defaults.scm"))
(define config.user          (if (file-exists? path:config.user)
                               (with-input-from-file path:config.user
                                                     (lambda () (read)))
                               '()))
(define config.defaults      (with-input-from-file path:config.defaults
                                                   (lambda () (read))))
(unless (and (list? config.user) (andmap pair? config.user))
  (error "invalid user config:"
         config.user (path->string (simplify-path path:config.user))))
(define config
  (let* ((user-keys (map car config.user))
         (user-defined? (lambda (kv) (member (car kv) user-keys))))
    (append config.user (filter-not user-defined? config.defaults))))
(define (config-ref key)
  (define kv (assoc key config))
  (unless kv (error "missing configuration key:" key))
  (cdr kv))
