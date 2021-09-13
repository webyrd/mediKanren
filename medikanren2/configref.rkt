#lang racket 
(require
    racket/runtime-path
)
(provide
    config
    config-ref
    load-config
    override-config
    override-dbkanren-defaults
)

(define (read/file path)  (with-input-from-file  path (lambda () (read))))

(define-runtime-path path:root ".")
(define (path/root relative-path) (build-path path:root relative-path))
(define (path/etc relative-path) (build-path path:root "etc" relative-path))
(define (path-simple path)        (path->string (simplify-path path)))

;; The configuration layers
(define config-by-cbranch (make-hash))

;; The active configuration, or #f if configuration needs to be rebuilt
(define box:config (box #f))
(define (config)
  (define cfg (unbox box:config))
  (cond (cfg cfg)
        (else (load-config #t)
              (unbox box:config))))
(define ((override-config-impl cbranch) config)
  (validate-config config)
  (hash-set! config-by-cbranch cbranch config)
  (set-box! box:config #f))
(define (config-ref key #:testing-dict (dict-config (config)))
  (define kv (assoc key dict-config))
  (unless kv (error "missing configuration key:" key))
  (cdr kv))
(define (valid-entry? kv)
  (and (pair? kv) (symbol? (car kv))))
(define (validate-config config)
  (unless (and (list? config) (andmap valid-entry? config))
    (error "invalid configuration:" config))
)
;;; config-combine
;; Default configs go last, and must contain a superset of all config keys.
;; Could be faster, but intended to only be run once at startup.
(define (config-combine . configs)
  (define (find k configs)
    (when (empty? configs)
      (error "config defaults must contain a superset of all other config keys"))
    (define kv (assoc k (car configs)))
    (if kv
      (cdr kv)
      (find k (cdr configs))))
  (define ks (map car (last configs)))
  (map (lambda (k)
    (cons k (find k configs)))
    ks)
)
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
      (else (hash-ref config-by-cbranch cbranch '())))))

(define (make-rebuild-flat with-user? verbose?)
  (apply config-combine
    (map (make-rebuild-by-cbranch with-user? verbose?) (reverse cbranches))))

(define (load-config verbose?)
  (define with-user? #t)
  (set-box! box:config (make-rebuild-flat with-user? verbose?)))

(define cbranches '(defaults dbkanren-defaults installer stage user override-test override))

;; Primarily for use in the repl, secondarily for use
;; in automated tests.  Use discouraged in applications.
(define override-config (override-config-impl 'override))
;; Propagates defaults from medikanren to dbkanren.
(define override-dbkanren-defaults (override-config-impl 'dbkanren-defaults))

(module+ test
  ; has required package:
  ;   raco pkg install chk
  ;
  ; how to run tests:
  ;   (cd medikanren && raco test configref.rkt)

  (require chk)

  ; test config-ref
  (chk
      #:= (config-ref 'foo #:testing-dict '((foo . 1))) 1)
  (chk
      #:do (config-ref 'foo #:testing-dict '((foo . 1)))
      #:t #t)
  (chk
      #:x (config-ref 'foo #:testing-dict '((bar . 1))) "missing configuration key")

  ; test validate-config
  (chk #:x (validate-config (vector)) "invalid configuration")
  (chk #:x (validate-config '(())) "invalid configuration")
  (chk #:x (validate-config '(("foo" . 1))) "invalid configuration")
  (chk
      #:do (validate-config '((foo . 1)))
      #:t #t)
  (chk
      #:do (validate-config (read/file (path:config.defaults)))
      #:t #t)


  ; test config-combine
  (chk #:=
    (config-ref 'foo #:testing-dict
      (config-combine '((foo . 1)) '((foo . 2)) ))
    1)
  (chk #:=
    (config-ref 'foo #:testing-dict
      (config-combine '((foo . 1)) '((foo . 2)) '((foo . 3)) ))
    1)
  (chk #:=
    (config-ref 'foo #:testing-dict
      (config-combine '() '((foo . 2)) ))
    2)
  (chk #:=
    (config-ref 'foo #:testing-dict
      (config-combine '((foo . 1)) '((foo . 2) (bar . 1)) ))
    1)

  ; test override-config
  #;(chk
    #:do ((override-config-impl 'override-test) '())
    #:do (override-config '((query-results.file-name-human . "last.txt")))
    #:= (config-ref 'query-results.file-name-human) "last.txt"
    )
  #;(chk
    #:do ((override-config-impl 'override-test) '((query-results.file-name-human . "last.txt")))
    #:do (override-config '())
    #:= (config-ref 'query-results.file-name-human) "last.txt"
    )
  #;(chk
    #:do ((override-config-impl 'override-test) '((query-results.file-name-human . "last.txt")))
    #:do (override-config '((query-results.file-name-human . "bob")))
    #:= (config-ref 'query-results.file-name-human) "bob"
    )
)