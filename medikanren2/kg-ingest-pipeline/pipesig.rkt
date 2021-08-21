#lang racket
(require racket/dict)
(require file/sha1)
(require chk)

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
  )
