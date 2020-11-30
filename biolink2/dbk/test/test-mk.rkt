#lang racket/base
(require "../mk.rkt" "../order.rkt" "../stream.rkt" "../table.rkt"
         racket/function racket/list racket/pretty racket/set)
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define (sort/any xs) (sort xs any<?))

(define-syntax-rule (test name e expected)
  (begin (printf "Testing ~s:\n" name)
         (let ((answer e))
           (unless (equal? answer expected)
             (pretty-print 'e)
             (printf "FAILED ~s:\n" name)
             (printf "  ANSWER:\n")
             (pretty-print answer)
             (printf "  EXPECTED:\n")
             (pretty-print expected)))))

(test 'appendo.forward
  (run* (z) (appendo '(1 2 3) '(4 5) z))
  '(((1 2 3 4 5))))
(test 'appendo.backward
  (run* (x y) (appendo x y '(1 2 3 4 5)))
  '((() (1 2 3 4 5))
    ((1) (2 3 4 5))
    ((1 2) (3 4 5))
    ((1 2 3) (4 5))
    ((1 2 3 4) (5))
    ((1 2 3 4 5) ())))
(test 'appendo.aggregate.1
  (run* (x y xsum)
    (appendo x y '(1 2 3 4 5))
    (:== xsum (x)
         (foldl + 0 x)))
  '((() (1 2 3 4 5)  0)
    ((1) (2 3 4 5)   1)
    ((1 2) (3 4 5)   3)
    ((1 2 3) (4 5)   6)
    ((1 2 3 4) (5)  10)
    ((1 2 3 4 5) () 15)))
(test 'appendo.aggregate.2
  (run* (x y xparts)
    (appendo x y '(1 2 3 4 5))
    (:== xparts (x)
         (run* (a b) (appendo a b x))))
  '((() (1 2 3 4 5) ((() ())))
    ((1) (2 3 4 5)  ((() (1))
                     ((1) ())))
    ((1 2) (3 4 5)  ((() (1 2))
                     ((1) (2))
                     ((1 2) ())))
    ((1 2 3) (4 5)  ((() (1 2 3))
                     ((1) (2 3))
                     ((1 2) (3))
                     ((1 2 3) ())))
    ((1 2 3 4) (5)  ((() (1 2 3 4))
                     ((1) (2 3 4))
                     ((1 2) (3 4))
                     ((1 2 3) (4))
                     ((1 2 3 4) ())))
    ((1 2 3 4 5) () ((() (1 2 3 4 5))
                     ((1) (2 3 4 5))
                     ((1 2) (3 4 5))
                     ((1 2 3) (4 5))
                     ((1 2 3 4) (5))
                     ((1 2 3 4 5) ())))))
(test 'appendo.aggregate.1.swapped
  (run* (x y xsum)
    (:== xsum (x)
         (foldl + 0 x))
    (appendo x y '(1 2 3 4 5)))
  '((() (1 2 3 4 5)  0)
    ((1) (2 3 4 5)   1)
    ((1 2) (3 4 5)   3)
    ((1 2 3) (4 5)   6)
    ((1 2 3 4) (5)  10)
    ((1 2 3 4 5) () 15)))
(test 'appendo.aggregate.2.swapped
  (run* (x y xparts)
    (:== xparts (x)
         (run* (a b) (appendo a b x)))
    (appendo x y '(1 2 3 4 5)))
  '((() (1 2 3 4 5) ((() ())))
    ((1) (2 3 4 5)  ((() (1))
                     ((1) ())))
    ((1 2) (3 4 5)  ((() (1 2))
                     ((1) (2))
                     ((1 2) ())))
    ((1 2 3) (4 5)  ((() (1 2 3))
                     ((1) (2 3))
                     ((1 2) (3))
                     ((1 2 3) ())))
    ((1 2 3 4) (5)  ((() (1 2 3 4))
                     ((1) (2 3 4))
                     ((1 2) (3 4))
                     ((1 2 3) (4))
                     ((1 2 3 4) ())))
    ((1 2 3 4 5) () ((() (1 2 3 4 5))
                     ((1) (2 3 4 5))
                     ((1 2) (3 4 5))
                     ((1 2 3) (4 5))
                     ((1 2 3 4) (5))
                     ((1 2 3 4 5) ())))))

(define-materialized-relation
  tripleo
  'attribute-names '(i x y z)
  'key-name        'i
  'source-vector   (vector #(a b c)
                           #(d e f)
                           #(g h i)))

(test 'tripleo.all
  (run* (i x y z) (tripleo i x y z))
  '((0 a b c) (1 d e f) (2 g h i)))
(test 'tripleo.filter-before
  (run* (i x y z)
    (conde ((== y 'e))
           ((== x 'g)))
    (tripleo i x y z))
  '((1 d e f) (2 g h i)))
(test 'tripleo.filter-before.key
  (run* (i x y z)
    (conde ((== y 'e))
           ((== x 'g))
           ((== i 3))
           ((== i 0)))
    (tripleo i x y z))
  '((1 d e f) (2 g h i) (0 a b c)))
(test 'tripleo.filter-before.key-only
  (run* (i x y z)
    (conde ((== y 'e))
           ((== x 'g))
           ((== i 3))
           ((== i 0)))
    (tripleo i x y z)
    (== i 0))
  '((0 a b c)))
(test 'tripleo.filter-after
  (run* (i x y z)
    (tripleo i x y z)
    (conde ((== i 0))
           ((== z 'i))))
  '((0 a b c) (2 g h i)))

;((hash-ref (relations-ref tripleo) 'cell)
 ;'set!
 ;(lambda args
   ;(constrain '(retrieve ((10 r s t) (11 u v w) (12 x y z))) args)))

;(test 'tripleo-rewired-filter-before
  ;(run* (i x y z)
    ;(conde ((== i 11))
           ;((== i 12)))
    ;(tripleo i x y z))
  ;'((11 u v w) (12 x y z)))
;(test 'tripleo-rewired-filter-after
  ;(run* (i x y z)
    ;(tripleo i x y z)
    ;(conde ((== i 10))
           ;((== i 12))))
  ;'((10 r s t) (12 x y z)))

;((hash-ref (relations-ref appendo) 'cell)
 ;'set!
 ;(lambda args
   ;(constrain '(retrieve ((10 20 30) (100 200 300))) args)))

;(test 'appendo-rewired
  ;(run* (a b c) (appendo a b c))
  ;'((10 20 30) (100 200 300)))

(define-materialized-relation
  triple2o
  'attribute-names '(x y z)
  'tables          '((y z x))
  'indexes         '((x))
  'source-vector   (vector #(a b  0)
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
                           #(c d 12)))

(test 'triple2o.all
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

(test 'triple2o.filter
  (list->set
    (run* (x y z)
      (conde ((== y 'a) (== z 'c))
             ((== y 'a) (== z 'd))
             ((== x '8))
             ((== y 'b) (== x '12))
             ((== y 'b) (== z 'f) (== x '9))
             ((== y 'b) (== z 'g) (== x '9))
             ((== y 'c))
             ((== y 'd)))
      (triple2o x y z)))
  (list->set
    '((4 a c)
      (5 a c)
      (6 a c)
      (8 b d)
      (9 b f)
      (11 c a)
      (12 c d))))

(test '=/=.atom.1
  (run* (x) (=/= 1 x))
  '(#s(cx (#s(var 0)) (=/=** ((#s(var 0) 1))))))
(test '=/=.atom.2
  (run* (x) (=/= x 2))
  '(#s(cx (#s(var 0)) (=/=** ((#s(var 0) 2))))))

(test '=/=.atom.==.1
  (run* (x) (== x 1) (=/= x 1))
  '())
(test '=/=.atom.==.2
  (run* (x) (=/= x 2) (== x 2))
  '())
(test '=/=.atom.==.3
  (run* (x) (=/= x 3) (== x 'not-3))
  '((not-3)))
(test '=/=.atom.==.4
  (run* (x) (== x 'not-4) (=/= x 4))
  '((not-4)))

(test '=/=.var.==.1
  (run* (x)
    (fresh (y)
      (=/= x y)
      (== x 1)
      (== y 1)))
  '())
(test '=/=.var.==.2
  (run* (x)
    (fresh (y)
      (== x 2)
      (== y 2)
      (=/= x y)))
  '())
(test '=/=.var.==.3
  (run* (x)
    (fresh (y)
      (== x 3)
      (=/= x y)
      (== y 3)))
  '())
(test '=/=.var.==.4
  (run* (x)
    (fresh (y z)
      (=/= x 4)
      (== x y)
      (== y z)
      (== z 4)))
  '())
(test '=/=.var.==.5
  (run* (x)
    (fresh (y z)
      (=/= x 5)
      (== y z)
      (== x y)
      (== z 5)))
  '())
(test '=/=.var.==.6
  (run* (x)
    (fresh (y)
      (=/= x y)
      (== x y)))
  '())
(test '=/=.var.==.7
  (run* (x)
    (fresh (y)
      (=/= x y)
      (== y x)))
  '())

(test '=/=.pair.==.1
  (run* (x)
    (=/= x '(1 . 2))
    (==  x '(1 . 2)))
  '())
(test '=/=.pair.==.2
  (run* (x)
    (fresh (y)
      (=/= x `(1 . ,y))
      (==  x `(1 . 2))
      (==  y 2)))
  '())
(test '=/=.pair.==.3
  (run* (x)
    (fresh (y)
      (==  x `(1 . 2))
      (=/= x `(1 . ,y))
      (==  y 2)))
  '())
(test '=/=.pair.==.4
  (run* (x)
    (fresh (y)
      (==  x `(1 . 2))
      (==  y 2)
      (=/= x `(1 . ,y))))
  '())
(test '=/=.pair.==.5
  (run* (x)
    (fresh (y)
      (=/= x `(1 . ,y))
      (==  y 2)
      (==  x `(1 . 2))))
  '())
(test '=/=.pair.==.6
  (run* (x)
    (fresh (y)
      (=/= `(,x .  1) `(0 . ,y))
      (==  `(,x . ,y) '(0 .  1))))
  '())
(test '=/=.pair.==.7
  (run* (x)
    (fresh (y)
      (==  `(,x . ,y) '(0 .  1))
      (=/= `(,x .  1) `(0 . ,y))))
  '())

(test '=/=.pair.=/=.1
  (run* (x)
    (=/= x '(1 . 2))
    (==  x '(0 . 2)))
  '(((0 . 2))))
(test '=/=.pair.=/=.2
  (run* (x)
    (fresh (y)
      (=/= x `(1 . ,y))
      (==  x `(1 . 2))
      (==  y 0)))
  '(((1 . 2))))
(test '=/=.pair.=/=.3
  (run* (x)
    (fresh (y)
      (==  x `(1 . 2))
      (=/= x `(1 . ,y))
      (==  y 0)))
  '(((1 . 2))))
(test '=/=.pair.=/=.4
  (run* (x)
    (fresh (y)
      (==  x `(1 . 2))
      (==  y 0)
      (=/= x `(1 . ,y))))
  '(((1 . 2))))
(test '=/=.pair.=/=.5
  (run* (x)
    (fresh (y)
      (=/= x `(1 . ,y))
      (==  y 0)
      (==  x `(1 . 2))))
  '(((1 . 2))))
(test '=/=.pair.=/=.6
  (run* (x)
    (fresh (y)
      (=/= `(,x .  1) `(0 . ,y))
      (==  `(,x . ,y) '(0 .  2))))
  '((0)))
(test '=/=.pair.=/=.7
  (run* (x)
    (fresh (y)
      (==  `(,x . ,y) '(0 .  2))
      (=/= `(,x .  1) `(0 . ,y))))
  '((0)))

(test '=/=.fresh.1
  (run* (x)
    (fresh (y)
      (=/= y 1)))
  '((#s(var 0))))
(test '=/=.fresh.2
  (run* (x)
    (fresh (y)
      (=/= x 0)
      (=/= y 1)))
  '(#s(cx (#s(var 0)) (=/=** ((#s(var 0) 0))))))
(test '=/=.fresh.3
  (run* (x)
    (fresh (y)
      (=/= x 0)
      (=/= x y)))
  '(#s(cx (#s(var 0)) (=/=** ((#s(var 0) 0))))))
(test '=/=.fresh.4
  (run* (x y)
    (fresh (z)
      (=/= `(,x . ,y) '(0 . 2))
      (=/= z 1)))
  '(#s(cx (#s(var 0) #s(var 1)) (=/=** ((#s(var 0) 0) (#s(var 1) 2))))))
(test '=/=.fresh.5
  (run* (x y)
    (fresh (z)
      (=/= `(,x ,y ,z) '(0 1 2))))
  '((#s(var 0) #s(var 1))))

(test 'membero.forward
  (run* () (membero 3 '(1 2 3 4 3 5)))
  '(()))
(test 'membero.backward
  (run* x (membero x '(1 2 3 4 3 5)))
  '(1 2 3 4 5))
(test 'not-membero.forward
  (run* () (not-membero 0 '(1 2 3 4 5)))
  '(()))
(test 'not-membero.backward
  (run* x (not-membero x '(1 2 3 4 5)) (== x 0))
  '(0))
(test 'uniqueo.1
  (run* () (uniqueo '(1 2 3 4 5)))
  '(()))
(test 'uniqueo.2
  (run* () (uniqueo '(1 2 3 4 2)))
  '())
(test 'removeo.forward
  (run* x (removeo '3 '(1 2 3 4 5) x))
  '((1 2 4 5)))
(test 'removeo.backward
  (run* x (removeo x '(1 2 3 4 5) '(1 2 4 5)))
  '(3))


;; More table testing

(define intersected-vectors
  (list (vector
          #(-1 0 no)
          #(-1 1 no)
          #(-1 2 no)
          #(-1 3 no)
          #(-1 4 no)
          #(-1 5 no)
          #(-1 6 no)
          #(-1 7 no)
          #(-1 8 no)
          #(-1 9 no)

          #(1 1 a0)
          #(1 2 b0)
          #(1 5 c0)
          #(3 4 d0)
          #(3 8 e0)
          #(3 8 e0.1)
          #(3 8 e0.2)

          #(4 0 no)
          #(4 1 no)
          #(4 2 no)
          #(4 3 no)
          #(4 4 no)
          #(4 5 no)
          #(4 6 no)
          #(4 7 no)
          #(4 8 no)
          #(4 9 no)

          #(6 0 f0)
          #(6 3 g0)
          #(6 5 h0)
          #(7 0 i0)
          #(7 2 j0)
          #(7 4 k0)
          #(7 6 l0)
          #(7 8 m0)
          #(7 9 n0)
          #(9 1 0)
          #(9 1 o0)
          #(9 2 p0)
          #(9 5 q0))
        (vector
          #(-1 0 no)
          #(-1 1 no)
          #(-1 2 no)
          #(-1 3 no)
          #(-1 4 no)
          #(-1 5 no)
          #(-1 6 no)
          #(-1 7 no)
          #(-1 8 no)
          #(-1 9 no)

          #(1 1 a1)
          #(1 2 b1)
          #(2 5 c1)  ; 0
          #(2 4 d1)  ; 0
          #(3 8 e1)
          #(3 8 e1.1)
          #(6 0 f1)
          #(6 3 g1)
          #(6 5 h1)
          #(7 0 i1)
          #(7 3 j1)  ; 1
          #(7 4 k1)
          #(7 6 l1)
          #(8 8 m1)  ; 0
          #(8 9 n1)  ; 0
          #(9 1 o1)
          #(9 1 o1.1)
          #(9 4 p1)  ; 1
          #(9 5 q1))
        (vector
          ;#(-1 0 no)
          ;#(-1 1 no)
          ;#(-1 2 no)
          ;#(-1 3 no)
          ;#(-1 4 no)
          ;#(-1 5 no)
          ;#(-1 6 no)
          ;#(-1 7 no)
          ;#(-1 8 no)
          ;#(-1 9 no)

          #(0 1 a2)  ; 0
          #(1 3 b2)  ; 1
          #(1 5 c2)  ; 1
          #(2 4 d2)
          #(3 8 e2)
          #(5 0 f2)  ; 0
          #(6 3 g2)
          #(6 5 h2)
          #(7 0 i2)
          #(7 3 j2)
          #(7 4 k2)
          #(7 4 k2.1)
          #(7 6 l2)
          #(8 8 m2)
          #(8 9 n2)
          #(9 1 o2)
          #(9 4 p2)
          #(9 5 q2)

          #(10 1 no)
          #(10 0 no)
          #(10 2 no)
          #(10 3 no)
          #(10 4 no)
          #(10 5 no)
          #(10 6 no)
          #(10 7 no)
          #(10 8 no)
          #(10 9 no)
          )))

(define intersected-tables
  (map (lambda (i v)
         (materialized-relation
           'relation-name   (string->symbol (format "intersected-table.~v" i))
           'attribute-names '(i n m x)
           'key-name        'i
           'source-vector   v))
       (range (length intersected-vectors))
       intersected-vectors))

(test 'table-ref
  (map (lambda (R) (run* (n m x) (R 0 n m x))) intersected-tables)
  '(((-1 0 no)) ((-1 0 no)) ((0 1 a2))))

(test 'table-intersection
  (sort/any
    (run* (n m a b c)
      (foldl (lambda (g0 g) (fresh () g g0)) (== #t #t)
             (map (lambda (v R) (fresh (i) (R i n m v))) (list a b c)
                  intersected-tables))))
  (sort/any
    '((3 8 e0   e1   e2)
      (3 8 e0   e1.1 e2)
      (3 8 e0.1 e1   e2)
      (3 8 e0.1 e1.1 e2)
      (3 8 e0.2 e1   e2)
      (3 8 e0.2 e1.1 e2)
      (6 3 g0   g1   g2)
      (6 5 h0   h1   h2)
      (7 0 i0   i1   i2)
      (7 4 k0   k1   k2)
      (7 4 k0   k1   k2.1)
      (7 6 l0   l1   l2)
      (9 1 0    o1   o2)
      (9 1 0    o1.1 o2)
      (9 1 o0   o1   o2)
      (9 1 o0   o1.1 o2)
      (9 5 q0   q1   q2))))
