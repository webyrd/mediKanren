#lang racket
(provide
    sjsexpr->string
    sjsexpr->bytes)
(require json)
(require chk)

#|
    Despite yaml being an isomorphism of json, the racket yaml and json packages bizarrely do not
    agree on whether strings (yaml) or symbols (json) are to be used as the representation for keys
    in racket.

    Escape hatch: keynorm-as-json

    A symbolic expression in racket may contain symbols as values, but the json package does
    not allow symbols as values.

    Escape hatch: valnorm-as-json
|#

(define (keynorm-as-json k)
    (cond
        ((string? k) (string->symbol k))
        ((symbol? k) k)
        (else k)))

(define (valnorm-as-json v)
    (cond
        ((symbol? v) (symbol->string v))
        (else v)))

(define (dictkeynorm keynorm valnorm expr)
  (define (walk-kvs kvs kvs-done)
    (if (null? kvs)
        (make-hash (reverse kvs-done))
        (let* ((kv (car kvs))
               (k (keynorm (car kv)))
               (v (walk (cdr kv))))
          (walk-kvs (cdr kvs) (cons (cons k v) kvs-done)))))
  (define (walk-list expr expr-done)
    (if (null? expr)
        (reverse expr-done)
        (walk-list (cdr expr) (cons (walk (car expr)) expr-done))))
  (define (walk expr)
    (cond 
      ((dict? expr)
       (let ((kvs (dict->list expr)))
         (walk-kvs kvs '())))
      ((list? expr)
       (walk-list expr '()))
      (else (valnorm expr))))
  (walk expr))

(define (sjsexpr->string expr)
  (jsexpr->string (dictkeynorm keynorm-as-json valnorm-as-json expr)))

(define (sjsexpr->bytes expr)
  (jsexpr->bytes (dictkeynorm keynorm-as-json valnorm-as-json expr)))

(module+ test
  (chk
    (#:=
      (sjsexpr->string `#hash(("hello" . "world")))
      "{\"hello\":\"world\"}"))
  (chk
    (#:=
      (sjsexpr->string `("hello" "world"))
      "[\"hello\",\"world\"]"))
  (chk
    (#:t (symbol? (car (dict-keys (car (dictkeynorm keynorm-as-json `(#hash(("hello" . "world"))))))))))
  (chk
    (#:t (symbol? (car (dict-keys (car (dictkeynorm keynorm-as-json `(#hash(("hello" . #hash(("hello" . "world"))))))))))))
)