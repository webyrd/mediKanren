#lang racket 
(require
    racket/runtime-path
)
(provide
    config-ref
    refresh-config
    validate-config
    config-combine
    configlayer-ref
    expose-configlayer
    set-build-thunk!
)

;; The configuration layers
(define config-by-cbranch (make-hash))
(define (configlayer-ref cb cb-default)
  (hash-ref config-by-cbranch cb cb-default))

;; The active configuration, or #f if configuration needs to be rebuilt
(define box:config (box #f))
(define box:build-config (box (lambda () '())))
(define (set-build-thunk! build-config)
  (set-box! box:build-config build-config))

(define (config-current)
  (define cfg (unbox box:config))
  (cond (cfg cfg)
        (else (refresh-config)
              (unbox box:config))))
(define ((expose-configlayer cbranch) config)
  (validate-config config)
  (hash-set! config-by-cbranch cbranch config)
  (set-box! box:config #f))
(define (config-ref key #:testing-dict (dict-config (config-current)))
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

(define (refresh-config)
  (set-box! box:config ((unbox box:build-config))))

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
    #:do ((expose-configlayer 'override-test) '())
    #:do (override-config '((query-results.file-name-human . "last.txt")))
    #:= (config-ref 'query-results.file-name-human) "last.txt"
    )
  #;(chk
    #:do ((expose-configlayer 'override-test) '((query-results.file-name-human . "last.txt")))
    #:do (override-config '())
    #:= (config-ref 'query-results.file-name-human) "last.txt"
    )
  #;(chk
    #:do ((expose-configlayer 'override-test) '((query-results.file-name-human . "last.txt")))
    #:do (override-config '((query-results.file-name-human . "bob")))
    #:= (config-ref 'query-results.file-name-human) "bob"
    )
)