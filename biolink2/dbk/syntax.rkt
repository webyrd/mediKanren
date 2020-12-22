#lang racket/base
(provide
  (struct-out make-query)
  (struct-out disj)
  (struct-out conj)
  (struct-out constrain)
  (struct-out ==/use)
  (struct-out var)
  relate retrieve

  make-relation relations relations-ref relations-set! relations-set*!
  relation letrec-relation define-relation
  relation/stream letrec-relation/stream define-relation/stream
  conj* disj* fresh conde :== query
  == =/= any<=o flooro +o *o string==byteso symbol==stringo functiono
  vector-lengtho vector-refo bytes-lengtho bytes-refo

  ground? term-vars
  make-pretty pretty)
(require racket/match racket/set racket/vector)

(struct query     (term g)                      #:prefab #:name make-query
                                                #:constructor-name make-query)
;; goals
(struct disj      (g1 g2)                       #:prefab)
(struct conj      (g1 g2)                       #:prefab)
(struct constrain (op terms)                    #:prefab)
(struct ==/use    (lhs-term args rhs-proc desc) #:prefab)
;; terms
(struct var       (name))

(define-syntax define-constraint
  (syntax-rules ()
    ((_ (op params ...)) (define (op params ...)
                           (constrain 'op (list params ...))))))
(define-constraint (==              t1 t2))
(define-constraint (=/=             t1 t2))
(define-constraint (any<=o          t1 t2))
(define-constraint (flooro          t1 t2))
(define-constraint (+o              t1 t2 t3))
(define-constraint (*o              t1 t2 t3))
(define-constraint (vector-lengtho  t l))
(define-constraint (vector-refo     t i x))
(define-constraint (bytes-lengtho   t l))
(define-constraint (bytes-refo      t i x))
(define-constraint (symbol==stringo t1 t2))
(define-constraint (string==byteso  t1 t2))  ;; as utf-8
(define-constraint (functiono       t1 t2))  ;; uninterpreted functional dependency
(define (retrieve stream args) (constrain `(retrieve ,stream) args))
(define (relate proc args) (constrain proc args))

(define relation-registry          (make-weak-hasheq '()))
(define (relations)                (hash->list relation-registry))
(define (relations-ref   proc)     (hash-ref relation-registry proc))
(define (relations-set!  proc k v) (relations-set*! proc `((,k . ,v))))
(define (relations-set*! proc alist)
  (hash-set! relation-registry proc
             (foldl (lambda (kv acc) (hash-set acc (car kv) (cdr kv)))
                    (relations-ref proc) alist)))
(define (make-relation name attributes)
  (define n ((make-syntax-introducer) (datum->syntax #f name)))
  (define r (eval-syntax #`(letrec ((#,n (lambda args (relate #,n args))))
                             #,n)))
  (hash-set! relation-registry r (make-immutable-hash
                                   `((name            . ,name)
                                     (attribute-names . ,attributes))))
  r)

(define-syntax relation
  (syntax-rules ()
    ((_ name (param ...) g ...)
     (let ((r (make-relation 'name '(param ...))))
       (relations-set! r 'expand (lambda (param ...) (fresh () g ...)))
       r))))
(define-syntax letrec-relation
  (syntax-rules ()
    ((_ (((name param ...) g ...) ...) body ...)
     (letrec ((name (relation name (param ...) g ...)) ...) body ...))))
(define-syntax define-relation
  (syntax-rules ()
    ((_ (name param ...) g ...)
     (define name (relation name (param ...) g ...)))))
(define-syntax relation/stream
  (syntax-rules ()
    ((_ name (param ...) stream)
     (relation name (param ...) (retrieve stream param ...)))))
(define-syntax letrec-relation/stream
  (syntax-rules ()
    ((_ (((name param ...) stream) ...) body ...)
     (letrec ((name (relation/stream name (param ...) stream)) ...)
       body ...))))
(define-syntax define-relation/stream
  (syntax-rules ()
    ((_ (name param ...) stream)
     (define name (relation/stream name (param ...) stream)))))
(define success (== #t #t))
(define failure (== #f #t))
(define (conj* . gs)
  (if (null? gs) success
    (foldl (lambda (g2 g1) (conj g1 g2)) (car gs) (cdr gs))))
(define (disj* . gs)
  (if (null? gs) failure
    (let loop ((g (car gs)) (gs (cdr gs)))
      (if (null? gs) g
        (disj g (loop (car gs) (cdr gs)))))))
(define-syntax let/fresh
  (syntax-rules ()
    ((_ (x ...) e ...) (let ((x (var 'x)) ...) e ...))))
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 gs ...) (let/fresh (x ...) (conj* g0 gs ...)))))
(define-syntax conde
  (syntax-rules ()
    ((_ (g gs ...) (h hs ...) ...)
     (disj* (conj* g gs ...) (conj* h hs ...) ...))))
(define-syntax :==
  (syntax-rules ()
    ((_ t (x ...) body ...) (==/use t (list x ...) (lambda (x ...) body ...)
                                    `((x ...) body ...)))))
(define-syntax query
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let/fresh (x ...) (make-query (list x ...) (conj* g0 gs ...))))
    ((_ x       g0 gs ...)
     (let/fresh (x)     (make-query x            (conj* g0 gs ...))))))

(define seteq.empty (seteq))
(define (term-vars t)
  (cond ((var?    t) (seteq t))
        ((pair?   t) (set-union (term-vars (car t)) (term-vars (cdr t))))
        ((vector? t) (apply set-union (map term-vars (vector->list t))))
        (else        seteq.empty)))
(define (ground? t)
  (cond ((var?    t) #f)
        ((pair?   t) (and (ground? (car t)) (ground? (cdr t))))
        ((vector? t) (andmap ground? (vector->list t)))
        (else        #t)))

(define (make-pretty)
  (define var=>id (make-hash))
  (define (pretty-term t)
    (cond ((pair? t)   (cons (pretty-term (car t)) (pretty-term (cdr t))))
          ((vector? t) (vector-map pretty-term t))
          ((var? t)    `#s(var ,(let ((id (hash-ref   var=>id t #f))
                                      (c  (hash-count var=>id)))
                                  (or id (begin (hash-set! var=>id t c) c)))))
          (else        t)))
  (define (pretty-goal g)
    (match g
      (`#s(disj ,g1 ,g2)         `#s(disj ,(pretty-goal g1) ,(pretty-goal g2)))
      (`#s(conj ,g1 ,g2)         `#s(conj ,(pretty-goal g1) ,(pretty-goal g2)))
      (`#s(constrain ,op ,terms) `(,op ,(map pretty-term terms)))
      (`#s(==/use ,lhs ,args ,rhs ,desc)
        ;(define (pretty-arg t) `',(pretty-term t))
        (define (pretty-arg t) (pretty-term t))
        `(:== ,(pretty-term lhs)
              #s(let ,(map list (car desc) (map pretty-arg args))
                  ,@(cdr desc))))))
  (lambda (x)
    (match x
      (`#s(query ,t ,g)
        `#s(query ,(pretty-term t) ,(pretty-goal g)))
      (_ (if (or (disj? x) (conj? x) (constrain? x) (==/use? x))
           (pretty-goal x) (pretty-term x))))))
(define (pretty x) ((make-pretty) x))
