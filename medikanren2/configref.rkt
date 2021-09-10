#lang racket 
(require
    racket/runtime-path
)
(provide
    config
    config-ref
    load-config
    override-config
)

(define (read/file path)  (with-input-from-file  path (lambda () (read))))

(define-runtime-path path:root ".")
(define (path/root relative-path) (build-path path:root relative-path))
(define (path-simple path)        (path->string (simplify-path path)))

(define box:config-override (box '()))
(define box:config (box #f))
(define (config)
  (define cfg (unbox box:config))
  (cond (cfg cfg)
        (else (load-config #t)
              (unbox box:config))))
;;; override-config
;; Set a set of config keys with higher precidence than "user config" config.scm
;; or "config.defaults.scm".  Primarily for use in the repl, secondarily for use
;; in automated tests.  Use discouraged in applications.
(define (override-config config)
  (validate-config config)
  (set-box! box:config-override config)
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
(define (path:config.user) (path/root "config.scm"))
(define (path:config.defaults) (path/root "config.defaults.scm"))
(define (load-config verbose?)
  (when verbose? (printf "loading default configuration: ~a\n"
                         (path-simple (path:config.defaults))))
  (when verbose? (printf "loading user configuration: ~a\n"
                         (path-simple (path:config.user))))
  (define config.user     (if (file-exists? (path:config.user))
                            (read/file (path:config.user))
                            '()))
  (define config.defaults (read/file (path:config.defaults)))
  (validate-config config.user)
  (set-box! box:config (config-combine (unbox box:config-override) config.user config.defaults)))

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
  (chk
    #:= (config-ref 'query-results.file-name-human) "last.txt"
    )
  (chk
    #:do (override-config '())
    #:= (config-ref 'query-results.file-name-human) "last.txt"
    )
  (chk
    #:do (override-config '((query-results.file-name-human . "bob")))
    #:= (config-ref 'query-results.file-name-human) "bob"
    )
)