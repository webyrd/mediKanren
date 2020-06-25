#lang racket/base
(require "mk.rkt" "relation.rkt" "table.rkt" racket/function racket/pretty)
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define-syntax-rule (test name e expected)
  (begin (printf "Testing ~s:\n" name)
         (pretty-print 'e)
         (let ((answer e))
           (unless (equal? answer expected)
             (printf "FAILED ~s:\n" name)
             (printf "  ANSWER:\n")
             (pretty-print answer)
             (printf "  EXPECTED:\n")
             (pretty-print expected)))))

(define-relation (appendo xs ys xsys)
  (conde ((== xs '()) (== ys xsys))
         ((fresh (a d res)
            (== `(,a . ,d)   xs)
            (== `(,a . ,res) xsys)
            (appendo d ys res)))))

(test 'appendo-forward
  (run* (z) (appendo '(1 2 3) '(4 5) z))
  '(((1 2 3 4 5))))
(test 'appendo-backward
  (run* (x y) (appendo x y '(1 2 3 4 5)))
  '((() (1 2 3 4 5))
    ((1) (2 3 4 5))
    ((1 2) (3 4 5))
    ((1 2 3) (4 5))
    ((1 2 3 4) (5))
    ((1 2 3 4 5) ())))

(define-relation/stream
  (tripleo i x y z)
  (thunk '((a b c)
           (d e f)
           (g h i))))

(test 'tripleo-all
  (run* (i x y z) (tripleo i x y z))
  '((0 a b c) (1 d e f) (2 g h i)))
(test 'tripleo-filter-before
  (run* (i x y z)
    (conde ((== y 'e))
           ((== x 'g)))
    (tripleo i x y z))
  '((1 d e f) (2 g h i)))
(test 'tripleo-filter-after
  (run* (i x y z)
    (tripleo i x y z)
    (conde ((== i 0))
           ((== z 'i))))
  '((0 a b c) (2 g h i)))

((hash-ref (relations-ref tripleo) 'cell)
 'set!
 (lambda args
   (constrain '(retrieve ((10 r s t) (11 u v w) (12 x y z))) args)))

(test 'tripleo-rewired-filter-before
  (run* (i x y z)
    (conde ((== i 11))
           ((== i 12)))
    (tripleo i x y z))
  '((11 u v w) (12 x y z)))
(test 'tripleo-rewired-filter-after
  (run* (i x y z)
    (tripleo i x y z)
    (conde ((== i 10))
           ((== i 12))))
  '((10 r s t) (12 x y z)))

((hash-ref (relations-ref appendo) 'cell)
 'set!
 (lambda args
   (constrain '(retrieve ((10 20 30) (100 200 300))) args)))

(test 'appendo-rewired
  (run* (a b c) (appendo a b c))
  '((10 20 30) (100 200 300)))

(define-relation/tables
  (triple2o x y z)
  (list (cons '(y z x) (table/vector
                         '(y z x) '(#f #f #f)
                         #(#(a b  0)
                           #(a b  1)
                           #(a b  2)
                           #(a b  3)
                           #(a c  4)
                           #(a c  5)
                           #(a c  6)
                           #(b a  7)
                           #(b d  8)
                           #(b f  9)
                           #(b q 10)
                           #(c a 11)
                           #(c d 12))))))

(test 'triple2o-all
  (run* (x y z) (triple2o x y z))
  '((0  a b)
    (1  a b)
    (2  a b)
    (3  a b)
    (4  a c)
    (5  a c)
    (6  a c)
    (7  b a)
    (8  b d)
    (9  b f)
    (10 b q)
    (11 c a)
    (12 c d)))

(test 'triple2o-filter
  (run* (x y z)
    (conde ((== y 'a) (== z 'c))
           ((== y 'a) (== z 'd))
           ((== y 'b) (== x '8))
           ((== y 'b) (== x '12))
           ((== y 'b) (== z 'f) (== x '9))
           ((== y 'b) (== z 'g) (== x '9))
           ((== y 'c))
           ((== y 'd)))
    (triple2o x y z))
  '((4 a c)
    (5 a c)
    (6 a c)
    (8 b d)
    (9 b f)
    (11 c a)
    (12 c d)))
