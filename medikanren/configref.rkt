#lang racket 
(require
    racket/runtime-path
)
(provide
    config
    config-ref
    load-config
)

(define (read/file path)  (with-input-from-file  path (lambda () (read))))

(define-runtime-path path:root ".")
(define (path/root relative-path) (build-path path:root relative-path))
(define (path-simple path)        (path->string (simplify-path path)))

(define box:config (box #f))
(define (config)
  (define cfg (unbox box:config))
  (cond (cfg cfg)
        (else (load-config #t #f)
              (unbox box:config))))
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
(define (config-combine config.user config.defaults)
  (define user-keys (map car config.user))
  (define (user-defined? kv) (member (car kv) user-keys))
  (append config.user (filter-not user-defined? config.defaults))
)
(define (path:config.user path:config) (or path:config (path/root "config.scm")))
(define (path:config.defaults) (path/root "config.defaults.scm"))
(define (load-config verbose? path:config)
  (when verbose? (printf "loading configuration defaults: ~a\n"
                         (path-simple (path:config.defaults))))
  (when verbose? (printf "loading configuration overrides: ~a\n"
                         (path-simple (path:config.user path:config))))
  (define config.user     (if (file-exists? (path:config.user path:config))
                            (read/file (path:config.user path:config))
                            '()))
  (define config.defaults (read/file (path:config.defaults)))
  (validate-config config.user)
  (set-box! box:config (config-combine config.user config.defaults)))

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
      (config-combine '() '((foo . 2)) ))
    2)
)