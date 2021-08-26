#lang racket
(provide
  psig-new
  psig-main-ref
  psig-extra-ref
  psig-main-set
  psig-extra-set
  psig-hash
  psig-payload
  (struct-out psig-t))
(require racket/dict)
(require file/sha1)
(require json)
(require chk)
(require "yaml-json-compat.rkt")

(struct psig
  (
   main
   extra
   ) #:transparent
  #:name psig-t
  )

(define (psig-new)
  (psig
    (make-immutable-hash)
    (make-immutable-hash)))

(define (psig-main-ref psig k)
  (hash-ref (psig-main psig) k))

(define (psig-extra-ref psig k)
  (hash-ref (psig-extra psig) k))

(define (psig-main-set psig1 k v)
  (struct-copy
    psig-t
    psig1
    (main (hash-set (psig-main psig1) k v))))

(define (psig-extra-set psig1 k v)
  (struct-copy
    psig-t
    psig1
    (extra (hash-set (psig-extra psig1) k v))))

(define (pipesig-flatten sig)
  (define (keynorm k)
    (cond
      ((string? k) k)
      ((symbol? k) (symbol->string k))
      (else k)))
  (define (kvcmp kv1 kv2)
    (string<? (keynorm (car kv1)) (keynorm (car kv2))))
  (define (walk-kvs kvs state)
    (if (null? kvs)
        state
        (let* ((kv (car kvs))
               (state `(> . ,(walk (cdr kv) `(,(car kv) < . ,state)))))
          (walk-kvs (cdr kvs) state))))
  (define (walk-list sig state)
    (if (null? sig)
        state
        (walk-list (cdr sig) (walk (car sig) state))))
  (define (walk sig state)
    (cond 
      ((dict? sig)
       (let ((kvs (sort (dict->list sig) kvcmp)))
         `(>> . ,(walk-kvs kvs `(<< . ,state)))))
      ((list? sig)
       `(> . ,(walk-list sig `(< . ,state))))
      (else `(,sig . ,state))))
  (reverse (walk sig '())))

(define (pipesig-hash sig)
  (define bytes
    (with-output-to-bytes
      (lambda ()
        (write (pipesig-flatten sig)))))
  (sha1 bytes))

(define (psig-hash psig)
  (pipesig-hash (psig-main psig)))

(define (psig-payload psig)
  (sjsexpr->bytes
    `#hash(("main" . ,(psig-main psig))
           ("extra" . ,(psig-extra psig)))))


(module+ test
  (chk
   (#:=
    (pipesig-flatten 'foo)
    '(foo)))
  
  (chk
   (#:=
    (pipesig-flatten `(3 4))
    '(< 3 4 >)))

  (chk
   (#:=
    (pipesig-flatten `#hash((foo . 1) (bar . 2) (baz . 3)))
    '(<< < bar 2 > < baz 3 > < foo 1 > >>)))

  (chk
   (#:=
    (pipesig-flatten `#hash(("foo" . 1) ("bar" . 2) ("baz" . 3)))
    '(<< < "bar" 2 > < "baz" 3 > < "foo" 1 > >>)))

  (chk
   (#:=
    (pipesig-flatten `#hash((foo . (3 4)) (bar . 2) (baz . #hash((a . b)))))
    '(<< < bar 2 > < baz << < a b > >> > < foo < 3 4 > > >>)))

  (chk
   (#:=
    (pipesig-hash `#hash((bar . 2) (baz . 3) (foo . 1)))
    (pipesig-hash `#hash((foo . 1) (bar . 2) (baz . 3)))))

  (chk
   (#:=
    (psig-hash (psig-main-set (psig `#hash((bar . 2) (baz . 3)) `#hash()) 'foo 1))
    (psig-hash (psig-main-set (psig `#hash((foo . 1) (bar . 2)) `#hash()) 'baz 3))))
  )
