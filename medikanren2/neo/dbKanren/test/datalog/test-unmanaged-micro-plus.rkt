#lang racket/base
(require "unmanaged-notation-micro-plus.rkt"
         racket/list racket/pretty racket/set)
(print-as-expression #f)
;(pretty-print-abbreviate-read-macros #f)

(define-syntax-rule
  (pretty-results example ...)
  (begin (let ((result (time example)))
           (pretty-write 'example)
           (pretty-write '==>)
           (pretty-write result)
           (newline)) ...))

(define (run-stratified-queries
          predicate=>compute predicate=>merge rules.query rule** facts)
  (let ((facts (run-stratified predicate=>compute predicate=>merge
                               (cons rules.query rule**) facts)))
    (map (lambda (predicate.query)
           (filter (lambda (fact) (eq? (car fact) predicate.query)) facts))
         (map caar rules.query))))

(define (run-queries rules.query rules facts)
  (run-stratified-queries (hash) (hash) rules.query (list rules) facts))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph traversal ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(pretty-results
  (run-queries
    '(((q1 x)   (path 'a x))
      ((q2 x)   (path x 'f))
      ((q3 x y) (path x y))
      ((q4 x y) (edge x y)))
    '(((path x y) (edge x y))
      ((path x z) (edge x y) (path y z)))
    '((edge a b)
      (edge b c)
      (edge d e)
      (edge e f)
      (edge b f)
      (edge f a)  ; comment this edge for an acyclic graph
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finite arithmetic ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define facts.+ (apply append (map (lambda (a)
                                     (map (lambda (b) `(+o ,a ,b ,(+ a b)))
                                          (range 50)))
                                   (range 50))))
(define facts.* (apply append (map (lambda (a)
                                     (map (lambda (b) `(*o ,a ,b ,(* a b)))
                                          (range 50)))
                                   (range 50))))
(define facts.< (apply append (map (lambda (a)
                                     (apply append (map (lambda (b) (if (< a b)
                                                                      `((<o ,a ,b))
                                                                      '()))
                                                        (range 50))))
                                   (range 50))))

(pretty-results
  (run-queries
    '(((q1 a b) (+o a b 7))
      ((q2 a b) (*o a b 7))
      ((q3 a b) (*o a b 18))
      ((q4 n)   (<o 0 n) (<o n 6)))
    '()
    (append facts.+ facts.* facts.<)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finite path length ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(pretty-results
  (run-stratified-queries
    (hash 'conso (lambda (a d ad)
                   (when (and (or (var? a) (var? d)) (var? ad))
                     (error "unsupported mode for conso" a d ad))
                   ((== (cons a d) ad) 'ignored)))
    (hash 'shortest-route-distance min)
    '(((q0 s t d)   (road  s t d))
      ((q1 s t d)   (shortest-route-distance s t d))
      ((q2 d)       (shortest-route-distance 'a 'd d))
      ((q3 s t d p) (shortest-route s t d p))
      ((q4 d p)     (shortest-route 'a 'd d p)))
    '((((shortest-route s t d p) (shortest-route-distance s t d)
                                 (road s t d)
                                 (conso t '() p.t)
                                 (conso s p.t p))
       ((shortest-route s t d p) (shortest-route-distance s t d)
                                 (road s mid d.0)
                                 (shortest-route mid t d.rest p.rest)
                                 (+o d.0 d.rest d)
                                 (conso s p.rest p)))
      (((shortest-route-distance s t d) (road s t d))
       ((shortest-route-distance s t d) (road s mid d.0)
                                        (shortest-route-distance mid t d.rest)
                                        (+o d.0 d.rest d))))
    (append facts.+ facts.<
            '((road a b 1)
              (road a c 7)
              (road b c 1)
              (road c d 1)
              (road d a 5)))))

;;;;;;;;;;;;;;;;;;;
;;; Aggregation ;;;
;;;;;;;;;;;;;;;;;;;

(pretty-results
  (run-stratified-queries
    (hash)
    (hash 'category-count +
          'order-total-cost +
          'order-total-count +
          'order-category-cost +
          'order-category-count +
          'vegan-order-count +
          'all-sales-total-amount +
          'all-sales-category-amount +
          'all-sales-category-count +)
    '(((q:category-count       'category: category 'count: count)                (category-count category count))
      ((q:order-total-cost     'order:    order    'cost:  cost)                 (order-total-cost order cost))
      ((q:order-total-count    'order:    order    'count: count)                (order-total-count order count))
      ((q:order-category-cost  'order:    order    'category: cat 'cost:  cost)  (order-category-cost order cat cost))
      ((q:order-category-count 'order:    order    'category: cat 'count: count) (order-category-count order cat count))
      ((q:vegan-orders      order) (order-vegan order))
      ((q:vegan-order-count count) (vegan-order-count count))
      ((q:all-sales-total-amount    'amount: amount)                (all-sales-total-amount amount))
      ((q:all-sales-category-amount 'category: cat 'amount: amount) (all-sales-category-amount cat amount))
      ((q:all-sales-category-count  'category: cat 'count:  count)  (all-sales-category-count  cat count)))
    '((run-once
        ((all-sales-total-amount amount) (all-sales-category-amount cat amount))
        ((vegan-order-count 0))
        ((vegan-order-count 1) (order-vegan order)))
      (((order-vegan order) (item order food) not (order-includes-animal-product order)))
      (run-once
        ((category-count cat 1) (food-category food cat))
        ((order-total-cost order amount) (item order food) (food-price food amount))
        ((order-total-count order 1)     (item order food))
        ((order-category-cost order cat amount) (item order food)
                                                (food-category food cat)
                                                (food-price food amount))
        ((order-category-count order cat 1) (item order food)
                                            (food-category food cat))
        ((all-sales-category-amount cat 0)      (food-category food cat))
        ((all-sales-category-amount cat amount) (item order food)
                                                (food-category food cat)
                                                (food-price food amount))
        ((all-sales-category-count cat 0) (food-category any cat))
        ((all-sales-category-count cat 1) (item order food)
                                          (food-category food cat)))
      (((order-includes-animal-product order) (item order food) (food-category food 'dairy))
       ((order-includes-animal-product order) (item order 'pizza))
       ((order-includes-animal-product order) (item order 'icecream))))
    '((food-category broccoli  produce)
      (food-category spinach   produce)
      (food-category mushrooms produce)
      (food-category eggplant  produce)
      (food-category tomatoes  produce)
      (food-category beans     canned)
      (food-category peas      canned)
      (food-category cheese    dairy)
      (food-category cream     dairy)
      (food-category butter    dairy)
      (food-category eggs      dairy)
      (food-category pizza     frozen)
      (food-category icecream  frozen)
      (food-category dosa      frozen)
      (food-category rice      grain)
      (food-category millet    grain)
      (food-price broccoli  4)
      (food-price spinach   3)
      (food-price mushrooms 5)
      (food-price eggplant  6)
      (food-price tomatoes  5)
      (food-price beans     2)
      (food-price peas      1)
      (food-price cheese   12)
      (food-price cream     6)
      (food-price butter    5)
      (food-price eggs      6)
      (food-price pizza    18)
      (food-price icecream  9)
      (food-price dosa      7)
      (food-price rice     12)
      (food-price millet    8)
      (item 3 broccoli)
      (item 3 mushrooms)
      (item 3 tomatoes)
      (item 3 cheese)
      (item 3 cream)
      (item 3 eggs)
      (item 3 rice)
      (item 999 tomatoes)
      (item 999 broccoli)
      (item 999 dosa)
      (item 999 rice)
      (item 11 spinach)
      (item 11 eggplant)
      (item 11 cream)
      (item 11 millet)
      (item 11 pizza)
      (item 7 spinach)
      (item 7 mushrooms)
      (item 7 eggplant)
      (item 7 cream)
      (item 7 eggs)
      (item 7 millet)
      (item 7 pizza))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Equivalence classes ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-equivalence-query facts.same)
  (run-stratified-queries
    (hash '<o (lambda (a b)
                (when (or (var? a) (var? b))
                  (error "unsupported mode for <o" a b))
                (lambda (S) (if (< a b) (list S) '()))))
    (hash 'eq min)
    '(((q0 x y) (same x y))
      ((q1 x y) (eq x y)))
    '((((eq0 a b) (same a b) (<o b a))
       ((eq0 a b) (same b a) (<o b a))
       ((eq a b) (eq0 a b))
       ((eq a c) (eq0 a b)
                 (eq b c)
                 (<o c a))
       ((eq a c) (eq0 b a)
                 (eq b c)
                 (<o c a))))
    facts.same))

(pretty-results
  (run-equivalence-query
    ;; \/-shaped graph
    '((same 7 5)
      (same 3 5)
      (same 3 1)
      (same 6 8)
      (same 4 6)
      (same 4 2)
      (same 0 2)
      (same 1 0))))

(pretty-results
  (run-equivalence-query
    ;; /\-shaped graph
    '((same 7 5)
      (same 3 5)
      (same 3 1)
      (same 6 8)
      (same 4 6)
      (same 4 2)
      (same 0 2)
      (same 7 8))))

(pretty-results
  (run-equivalence-query
    ;; X-shaped graph
    '((same 17 15)
      (same 13 15)
      (same 13 11)
      (same 16 18)
      (same 14 16)
      (same 14 12)
      (same 10 12)
      (same 11 10)
      (same 9 11)
      (same 7 9)
      (same 7 5)
      (same 3 5)
      (same 3 1)
      (same 10 8)
      (same 6 8)
      (same 4 6)
      (same 4 2)
      (same 0 2))))

(pretty-results
  (run-equivalence-query
    '(;; class 5
      (same 5  10)
      (same 15 10)
      (same 25 10)
      (same 20 15)
      (same 25 35)
      (same 20 30)
      ;;; class 1005
      (same 1005  10010)
      (same 10015 10010)
      (same 10025 10010)
      (same 10020 10015)
      (same 10025 10035)
      (same 10020 10030))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strongly-connected components and stratification ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Downward directed graph with some cycles:
;      A       B
;     / \     / \
;    C   D*  E   F*
;    |    \ / \  |
;    J*   GHI  \ KL
;     \  /  \   X \
;      M     NO  \P
;      |       \ / \
;      Q        R   S

(define facts.dg
  (append (map (lambda (n) `(node ,n)) '(A B C D E F G H I J K L M N O P Q R S))
          '((depends A C)
            (depends A D)
            (depends B E)
            (depends B F)

            (depends D D)
            (depends F F)

            (depends C J)
            (depends D G)
            (depends E G)
            (depends F L)

            (depends J J)
            (depends G H)
            (depends G I)
            (depends H G)
            (depends I H)
            (depends K L)
            (depends L K)

            (depends J M)
            (depends H M)
            (depends I N)
            (depends K N)
            (depends L P)
            (depends E P)

            (depends N O)
            (depends O N)

            (depends M Q)
            (depends O R)
            (depends P R)
            (depends P S))))

(pretty-results
  (run-stratified-queries
    (hash 'symbol< (lambda (a b)
                     (when (or (var? a) (var? b))
                       (error "unsupported mode for symbol<o" a b))
                     (lambda (S) (if (or (symbol<? a b)) (list S) '())))
          '+o       (lambda (a b a+b)
                      ((cond ((and (rational? a) (rational? b))   (== a+b (+ a b)))
                             ((and (rational? a) (rational? a+b)) (== b (- a+b a)))
                             ((and (rational? b) (rational? a+b)) (== a (- a+b b)))
                             (else (error "unsupported mode for +o" a b a+b)))
                       'ignored)))
    (hash 'stratum max)
    '(((q:scc x y) (scc x y) not (scc-child x))
      ((q:scc-child x) (scc-child x))
      ((q:stratum level x) (stratum x level) not (scc-child x)))
    '((((stratum a 0) (node a) not (dependent a))
       ((stratum a n) (depends a b)
                      (stratum b m)
                      (+o m 1 n)
                      not
                      (== a b)
                      (scc a b)
                      (scc b a))
       ((stratum a n) (scc a b) (stratum b n))
       ((stratum a n) (scc b a) (stratum b n)))
      (((scc a b) (depends+ a b) (depends+ b a) (symbol< a b))
       ((scc-child a) (scc b a))
       ((depends+ a b) (depends a b))
       ((depends+ a c) (depends a b) (depends+ b c))
       ((dependent a) (depends a b))
       ((== a a) (node a))))
    facts.dg))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Mutable counter ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(define count.current 0)
(define rules.count '(((next-count next) (+o current 1 next) (count current))))
(define (current-count-state)
  (run-stratified (hash) (hash) (list rules.count)
                  (append facts.+
                          `((count ,count.current)))))
(define (state-extract facts.count predicate)
  (cadar (filter (lambda (fact) (eq? (car fact) predicate)) facts.count)))

(define (now  st) (state-extract st 'count))
(define (next st) (state-extract st 'next-count))

(define (increment-count!)
  (let ((st (current-count-state)))
    (pretty-write `(current count: ,(now st)))
    (set! count.current (next st))
    (pretty-write `(next count: ,(next st)))))

(for-each (lambda (_) (increment-count!)) (range 10))
