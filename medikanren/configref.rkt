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
(define (load-config verbose? path:config)
  (define path:config.user     (or path:config (path/root "config.scm")))
  (define path:config.defaults (path/root "config.defaults.scm"))
  (when verbose? (printf "loading configuration defaults: ~a\n"
                         (path-simple path:config.defaults)))
  (when verbose? (printf "loading configuration overrides: ~a\n"
                         (path-simple path:config.user)))
  (define config.user     (if (file-exists? path:config.user)
                            (read/file path:config.user)
                            '()))
  (define config.defaults (read/file path:config.defaults))
  (unless (and (list? config.user) (andmap pair? config.user))
    (error "invalid configuration overrides:" config.user))
  (define user-keys (map car config.user))
  (define (user-defined? kv) (member (car kv) user-keys))
  (set-box! box:config
            (append config.user (filter-not user-defined? config.defaults))))

(module+ test
  ; has required package:
  ;   raco pkg install chk
  ;
  ; how to run tests:
  ;   (cd medikanren && raco test configref.rkt)

  (require chk)

  (define (does-throw thunk)
      (with-handlers ([exn:fail?
                    (λ (e) #t)])
      (thunk)
      #f)
  )

  ; test the does-throw function
  (chk
      #:t (not (does-throw (lambda () 1))))
  (chk
      #:t (does-throw (lambda () (error "an error"))))

  ; test config-ref
  (chk
      #:= (config-ref "foo" #:testing-dict '(("foo" . 1))) 1)
  (chk
      #:t (not (does-throw (lambda () (config-ref "foo" #:testing-dict '(("foo" . 1)))))))
  (chk
      #:t (does-throw (lambda () (config-ref "foo" #:testing-dict '(("bar" . 1))))))

)