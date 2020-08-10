#lang racket/base
(provide
  (struct-out make-query)
  (struct-out make-use)
  (struct-out disj)
  (struct-out conj)
  (struct-out constrain)
  (struct-out var)
  ground?

  make-relation-proc relations relations-ref relations-set!
  relation letrec-relations define-relation
  conj* disj* fresh conde use query run^ run run*
  == =/= absento symbolo numbero stringo
  <=o +o *o string<=o string-appendo string-symbolo string-numbero
  walk* retrieve/dfs

  pretty-query pretty-goal pretty-term
  )

(require "method.rkt" "stream.rkt" racket/function racket/match racket/vector)

(struct query     (g var desc)     #:prefab #:name make-query
                                   #:constructor-name make-query)
(struct use       (proc args desc) #:prefab #:name make-use
                                   #:constructor-name make-use)
(struct disj      (g1 g2)          #:prefab)
(struct conj      (g1 g2)          #:prefab)
(struct constrain (op terms)       #:prefab)

(define-syntax define-constraint
  (syntax-rules ()
    ((_ (op params ...)) (define (op params ...)
                           (constrain 'op (list params ...))))))
(define-constraint (==             t1 t2))
(define-constraint (=/=            t1 t2))
(define-constraint (absento        t))
(define-constraint (symbolo        t))
(define-constraint (numbero        t))
(define-constraint (stringo        t))
(define-constraint (<=o            t1 t2))
(define-constraint (+o             t1 t2 t3))
(define-constraint (*o             t1 t2 t3))
(define-constraint (string<=o      t1 t2))
(define-constraint (string-appendo t1 t2 t3))
(define-constraint (string-symbolo t1 t2))
(define-constraint (string-numbero t1 t2))
(define (relate proc args) (constrain `(relate ,proc) args))

(define relation-registry     (make-weak-hasheq '()))
(define (relations)           (hash->list relation-registry))
(define (relations-ref  proc) (hash-ref relation-registry proc))
(define (relations-set! proc k v)
  (hash-set! relation-registry proc (hash-set (relations-ref proc) k v)))
(define (relations-register! proc name attributes)
  (hash-set! relation-registry proc
             (make-immutable-hash
               `((name                       . ,name)
                 (expand                     . #f)
                 (attribute-names            . ,attributes)
                 (attribute-types            . #f)
                 (integrity-constraints      . #f)
                 (location                   . #f)
                 (monotonic-dependencies     . #f)
                 (non-monotonic-dependencies . #f)
                 (analysis                   . #f)))))

(define (make-relation-proc name attributes)
  (define n ((make-syntax-introducer) (datum->syntax #f name)))
  (eval-syntax
    #`(letrec ((#,n (lambda args (relate #,n args))))
        (relations-register! #,n '#,name '(#,@attributes))
        #,n)))

(define-syntax relation
  (syntax-rules ()
    ((_ name (param ...) g ...)
     (let ((r (make-relation-proc 'name '(param ...))))
       (relations-set! r 'expand (lambda (param ...) (fresh () g ...)))
       r))))
(define-syntax letrec-relations
  (syntax-rules ()
    ((_ (((name param ...) g ...) ...) body ...)
     (letrec ((name (relation name (param ...) g ...)) ...) body ...))))
(define-syntax define-relation
  (syntax-rules ()
    ((_ (name param ...) g ...)
     (define name (relation name (param ...) g ...)))))
(define succeed (== #t #t))
(define fail    (== #f #t))
(define-syntax conj*
  (syntax-rules ()
    ((_)           succeed)
    ((_ g)         g)
    ((_ g0 gs ...) (conj g0 (conj* gs ...)))))
(define-syntax disj*
  (syntax-rules ()
    ((_)           fail)
    ((_ g)         g)
    ((_ g0 gs ...) (disj g0 (disj* gs ...)))))
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let ((x (var/fresh 'x)) ...) (conj* g0 gs ...)))))
(define-syntax conde
  (syntax-rules ()
    ((_ (g gs ...) (h hs ...) ...)
     (disj* (conj* g gs ...) (conj* h hs ...) ...))))
(define-syntax use
  (syntax-rules ()
    ((_ (x ...) body ...) (make-use (lambda (x ...) body ...)
                                    (list x ...)
                                    `((x ...) body ...)))))
(define-syntax query
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let ((initial-var (var/fresh #f)))
       (make-query (fresh (x ...) (== (list x ...) initial-var) g0 gs ...)
                   initial-var
                   `((x ...) g0 gs ...))))))
(define-syntax run^
  (syntax-rules () ((_   body ...) (query->stream (query  body ...)))))
(define-syntax run
  (syntax-rules () ((_ n body ...) (s-take n      (run^   body ...)))))
(define-syntax run*
  (syntax-rules () ((_   body ...)                (run #f body ...))))

;; TODO: move beyond DFS once other strategies are ready
(define (query->stream q) ((query->dfs q) (state-empty)))
(define (query->dfs q)
  (match-define `#s(query ,g ,x ,desc) q)
  (define (return st) (let ((result (pretty-term x)))
                        (state-undo! st)
                        (list result)))
  (goal->dfs g return))
(define (fail/dfs st) (state-undo! st) '())
(define (mplus/dfs k1 k2) (lambda (st) (s-append (k1 (state-new st))
                                                 (thunk (k2 st)))))
(define (retrieve/dfs k s args)
  (lambda (st) (let ((s (s-force s)))
                 ((if (null? s) fail/dfs
                    (mplus/dfs (goal->dfs (== (car s) args) k)
                               (retrieve/dfs k (cdr s) args)))
                  st))))
(define (goal->dfs g k)
  (define loop goal->dfs)
  (match g
    (`#s(conj ,g1 ,g2) (loop g1 (loop g2 k)))
    (`#s(disj ,g1 ,g2) (mplus/dfs (loop g1 k) (loop g2 k)))
    (`#s(constrain (relate ,proc) ,args)
      (define r (relations-ref proc))
      (define apply/dfs (hash-ref r 'apply/dfs #f))
      (cond (apply/dfs (apply/dfs k args))
            (else (define ex (hash-ref r 'expand #f))
                  (unless ex (error "no interpretation for:" proc args))
                  (lambda (st) ((loop (apply ex (walk* args)) k) st)))))
    (`#s(constrain == (,t1 ,t2))
      (lambda (st) ((if (unify* st t1 t2) k fail/dfs) st)))))

(struct state (assignments constraints) #:mutable)
(define (state-empty)  (state '() '()))
;; TODO: what should be preserved?  Should this link to the parent state?
(define (state-new st) (state-empty))
(define (state-assign! st x t)
  (set-state-assignments! st (cons (cons x (var-value x))
                                   (state-assignments st)))
  (set-var-value! x t))
(define (state-undo! st)
  (set-state-assignments!
    st (map (lambda (kv) (let* ((x (car kv)) (t (var-value x)))
                           (set-var-value! x (cdr kv))
                           (cons x t)))
            (state-assignments st))))
(define (state-redo! st) (state-undo! st))  ;; coincidentally, for now

;; TODO: variable lattice attributes supporting general constraints
#|
* type domains: #t top, #f bottom, lattice vector for domain sums
  * (), #t, #f domains need no representation beyond being top or bottom
  * pair domains are all represented as concrete values
    * though pairs may contain variables
  * symbol, string, bytes, and vector domains are represented as discrete sets
    * discrete sets are sorted lists of concrete values
      * though vectors may contain variables
  * number domains are represented as interval sets (ordered ranges)
|#
;(struct vspec (domain constraints) #:prefab)
;(define vtop (vspec #t '()))
;; TODO: register constrained/specified variables in a priority queue?

(struct var (name (value #:mutable)) #:prefab)
(define (var/fresh name) (var name (void)))  ;; TODO: use TOP instead of void
(define (var-assign! st x t) (and (not (occurs? x t)) (state-assign! st x t)))
(define (var-walk vr)
  (let ((val (var-value vr)))
    (cond ((var?  val) (let ((val^ (var-walk val)))
                         (unless (eq? val val^) (set-var-value! vr val^))
                         val^))
          ((void? val) vr)
          (else        val))))
(define (walk tm) (if (var? tm) (var-walk tm) tm))
(define (walk* t)
  (let ((t (walk t)))
    (cond ((pair? t)   (cons (walk* (car t)) (walk* (cdr t))))
          ((vector? t) (vector-map walk* t))
          ((use? t)    (apply (use-proc t) (walk* (use-args t))))
          (else        t))))
(define (occurs? x t)
  (cond ((pair? t)   (or (occurs? x (walk (car t)))
                         (occurs? x (walk (cdr t)))))
        ((vector? t) (let loop ((i (- (vector-length t) 1)))
                       (and (<= 0 i) (or (occurs? x (walk (vector-ref t i)))
                                         (loop (- i 1))))))
        (else        (eq? x t))))
(define (unify* st t1 t2) (unify st (walk* t1) (walk* t2)))
(define (unify st t1 t2)
  (let ((t1 (walk t1)) (t2 (walk t2)))
    (cond ((eqv? t1 t2) #t)
          ((var? t1)    (var-assign! st t1 t2))
          ((var? t2)    (var-assign! st t2 t1))
          ((pair? t1)   (and (pair? t2)
                             (unify st (car t1) (car t2))
                             (unify st (cdr t1) (cdr t2))))
          ((vector? t1) (and (vector? t2)
                             (= (vector-length t1) (vector-length t2))
                             (let loop ((i (- (vector-length t1) 1)))
                               (or (< i 0) (and (unify st
                                                       (vector-ref t1 i)
                                                       (vector-ref t2 i))
                                                (loop (- i 1)))))))
          ((string? t1) (and (string? t2) (string=? t1 t2)))
          (else         #f))))
(define (ground? t)
  (cond ((var?    t)  #f)
        ((pair?   t) (and (ground? (car t)) (ground? (cdr t))))
        ((vector? t) (andmap ground? (vector->list t)))
        (else        #t)))
;; TODO: constraint satisfaction

(define (pretty-printer)
  (define st (state-empty))
  (define var-count 0)
  (define (pretty-var x)
    (define v `#s(var ,(var-name x) ,var-count))
    (set! var-count (+ var-count 1))
    (var-assign! st x v)
    v)
  (define (pretty-term t)
    (let ((t (walk t)))
      (cond ((pair? t)   (cons (pretty-term (car t)) (pretty-term (cdr t))))
            ((vector? t) (vector-map pretty-term t))
            ((var? t)    (pretty-var t))
            ((use? t)    `(let ,(map list
                                     (car (use-desc t))
                                     (map pretty-term (use-args t)))
                            . ,(cdr (use-desc t))))
            (else        t))))
  (define (pretty-goal g)
    (match g
      (`#s(disj ,g1 ,g2)         `(disj ,(pretty-goal g1) ,(pretty-goal g2)))
      (`#s(conj ,g1 ,g2)         `(conj ,(pretty-goal g1) ,(pretty-goal g2)))
      (`#s(constrain ,op ,terms) `(,op . ,(map pretty-term terms)))
      (`#s(relate ,_ ,args (,_ . ,name))
        `(relate ,name . ,(map pretty-term args)))))
  (define (pretty-query q)
    (match q
      (`#s(query ,g ,x (,params . ,_))
        `(query ,params ,(pretty-term x) ,(pretty-goal g)))))
  (define (return x) (state-undo! st) x)
  (method-lambda
    ((query q) (return (pretty-query q)))
    ((term t)  (return (pretty-term t)))
    ((goal g)  (return (pretty-goal g)))))

(define (pretty-query q) ((pretty-printer) 'query q))
(define (pretty-goal  g) ((pretty-printer) 'goal  g))
(define (pretty-term  t) ((pretty-printer) 'term  t))
