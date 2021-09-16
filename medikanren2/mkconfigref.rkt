#lang racket
(require
    racket/runtime-path
)
(provide
    config-ref
    load-config
    override-config
    override-dbkanren-defaults
)
(require "configref.rkt")

(define box:verbose? (box #t))
(define (set-verbose! b)
  (set-box! box:verbose? b))

(define (read/file path)  (with-input-from-file  path (lambda () (read))))

(define-runtime-path path:root ".")
(define (path/root relative-path) (build-path path:root relative-path))
(define (path/etc relative-path) (build-path path:root "etc" relative-path))
(define (path-simple path)        (path->string (simplify-path path)))

(define (env-ref/utf-8 k)
  (define v (environment-variables-ref (current-environment-variables) (string->bytes/utf-8 k)))
  (if v
    (bytes->string/utf-8 v)
    #f))
(define (path:config.user migrated?)
  (if migrated?
    (path/etc "config.scm")
    ;; BEGIN TEMPORARY: migrated-to-new-db-versioning:
    (path/root "config.scm")))
    ;; END TEMPORARY
(define (path:config.stage.prod) (path/etc "config.stage.prod.scm"))
(define (path:config.stage.dev) (path/etc "config.stage.dev.scm"))
(define (path:config.installer) (path/etc "config.installer.scm"))
(define (path:config.defaults) (path/etc "config.defaults.scm"))
(define (make-config-user migrated? verbose?)
  (when verbose? (printf "loading user configuration: ~a\n"
                         (path-simple (path:config.user migrated?))))
  (define config.user     (if (file-exists? (path:config.user migrated?))
                            (read/file (path:config.user migrated?))
                            '()))
  (validate-config config.user)
  config.user)

(define (make-rebuild-by-cbranch with-user? verbose?)
  (when verbose? (printf "loading default configuration: ~a\n"
                         (path-simple (path:config.defaults))))
  (define config.user
    ;; BEGIN TEMPORARY: migrated-to-new-db-versioning:
    (if with-user?
      (let* (
          (h (make-rebuild-flat #f #f))
          (migrated? (dict-ref h 'migrated-to-new-db-versioning)))
    ;; END TEMPORARY
        (make-config-user migrated? verbose?))
      `()))
  (define config.stage
    (let* (
        (stage (env-ref/utf-8 "MK_STAGE"))
        (path1
          (cond
            ((equal? stage "prod") (path:config.stage.prod))
            ((equal? stage "dev") (path:config.stage.dev))
            ((not stage) (path:config.stage.dev))
            (else
              (printf "***Warning*** unknown MK_STAGE value: ~a" stage)
              (path:config.stage.dev)))))
      ;; No fallbacks for this file if missing.  Stage files should be in revision control
      (read/file path1)))
  (define config.installer (if (file-exists? (path:config.installer))
                              (read/file (path:config.installer))
                              '()))
  (define config.defaults (read/file (path:config.defaults)))
  (lambda (cbranch)
    (case cbranch
      ('user config.user)
      ('defaults config.defaults)
      ('installer config.installer)
      ('stage config.stage)
      (else (configlayer-ref cbranch '())))))

(define cbranches '(defaults dbkanren-defaults installer stage user override-test override))

(define (make-rebuild-flat with-user? verbose?)
  (apply config-combine
    (map (make-rebuild-by-cbranch with-user? verbose?) (reverse cbranches))))

(define (load-config verbose?)
  (set-verbose! verbose?)
  (refresh-config))

(let ((with-user? #t))
  (set-build-thunk! (lambda () (make-rebuild-flat with-user? (unbox box:verbose?)))))

;; Primarily for use in the repl, secondarily for use
;; in automated tests.  Use discouraged in applications.
(define override-config (expose-configlayer 'override))
;; Propagates defaults from medikanren to dbkanren.
(define override-dbkanren-defaults (expose-configlayer 'dbkanren-defaults))

(module+ test
  ; has required package:
  ;   raco pkg install chk
  ;
  ; how to run tests:
  ;   (cd medikanren && raco test configref.rkt)

  (require chk)

  (chk
      #:do (validate-config (read/file (path:config.defaults)))
      #:t #t)
)