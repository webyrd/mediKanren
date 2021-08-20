#lang racket
(require yaml)
(require chk)
(provide
    tagyaml->string)

;;; Cajole racket yaml into emitting a yaml "tag" without having to make a custom yaml representer
;;; for each call.
(define (tagyaml->string tag yamlexpr)
  (struct foo (bar))
  (define (rep y)
        (represent-mapping tag (foo-bar y)))
  (parameterize
    ((yaml-representers
      (list
        (yaml-representer
          (lambda (x) (foo? x))
          rep))))
    (with-output-to-string
      (lambda ()
        (write-yaml
            (map foo yamlexpr)
            (current-output-port)
            #:style 'block
            #:explicit-start? #t)))))


(module+ test
  (chk
    (#:=
        (tagyaml->string "!foo" `(#hash(("bar" . "baz") ("quux" . #hash(("a" . "b"))))))
        "---\n- !foo\n  bar: baz\n  quux:\n    a: b\n"))
  (chk
    (#:=
        (tagyaml->string "!foo" `(#hash(("bar" . "baz")) #hash(("bar" . "baz"))))
        "---\n- !foo\n  bar: baz\n- !foo\n  bar: baz\n"))
  )
