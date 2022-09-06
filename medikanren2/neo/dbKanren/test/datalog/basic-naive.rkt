#lang racket/base
(provide
  facts
  ==
  fresh
  conde
  run*
  define-relation)
(require racket/list racket/match racket/set racket/struct)

;; A naive Datalog implementation that can be used as a baseline

;; Terms:
;; - Constants and variables
;; Formulas:
;; - Existential quantification, conjunction, disjunction
;;   - via fresh and conde
;;   - Negation is not supported in this basic implementation
;; - Relation calls
;; - Equality constraints
;; - run*
;; Relations:
;; - via define-relation and define-relation/facts (for convenience)

;; Variables should not be nested within data structures, but this is not checked.
;; All relations should be range restricted for safety, but this is not checked.

(struct var (name) #:prefab)

(define subst.empty (hash))
(define (unpack subst names)
  (map (lambda (n) (hash-ref subst n (lambda () (error "unbound variable" n)))) names))
(define (shadow subst names)
  (foldl (lambda (n s) (hash-remove s n)) subst names))
(define (assign subst x t)
  (and (not (occurs? subst x t))
       (hash-set subst (var-name x) t)))
(define (walk subst x)
  (let loop ((x x))
    (if (var? x)
      (let ((y (hash-ref subst (var-name x) x)))
        (if (equal? x y)
          x
          (loop y)))
      x)))
(define (occurs? subst x t)
  (let ((x (walk subst x)) (t (walk subst t)))
    (or (equal? x t)
        (and (pair? t)
             (or (occurs? subst x (car t))
                 (occurs? subst x (cdr t))))
        (and (vector? t)
             (occurs? subst x (vector->list t))))))
(define (unify subst u v)
  (let ((u (walk subst u)) (v (walk subst v)))
    (cond ((eqv? u v)  subst)
          ((var? u)    (if (and (var? v) (equal? (var-name u) (var-name v)))
                         subst
                         (assign subst u v)))
          ((var? v)    (assign subst v u))
          ((pair? u)   (and (pair? v)
                            (let ((subst (unify subst (car u) (car v))))
                              (and subst
                                   (unify subst (cdr u) (cdr v))))))
          ((vector? u) (and (vector? v)
                            (unify subst (vector->list u) (vector->list v))))
          (else        (and (equal? u v) subst)))))

(struct relation (name attrs box.current qthunk)
        #:methods gen:custom-write
        ((define write-proc (make-constructor-style-printer
                              (lambda (r) 'relation)
                              (lambda (r) (list (cons (relation-name r) (relation-attrs r)))))))
        #:property prop:procedure
        (lambda (r . args)
          (unless (= (length (relation-attrs r)) (length args))
            (error "relation called with invalid number of arguments" r args))
          `(relate ,r ,args)))

(define (relation-current r) (unbox (relation-box.current r)))
(define (relation-next    r) (query-eval (relation-query  r)))
(define (relation-query   r) ((relation-qthunk            r)))

(define (query-dependencies q)
  (let loop ((fm q) (rs (set)))
    (match fm
      (`(and   . ,fms) (foldl loop rs fms))
      (`(or    . ,fms) (foldl loop rs fms))
      (`(relate ,r ,_) (if (set-member? rs r)
                         rs
                         (loop (relation-query r) (set-add rs r))))
      (`(exist ,_ ,fm) (loop fm rs))
      (`(query ,_ ,fm) (loop fm rs))
      (`(==    ,_ ,_)  rs))))

(define (query-eval q)
  (match q
    (`(query ,var-names ,fm)
      (list->set
        (map (lambda (subst) (unpack subst var-names))
             (let loop ((fm fm) (subst subst.empty))
               (match fm
                 (`(and ,fm . ,fms)  (let ((substs (loop fm subst)))
                                       (append* (map (lambda (subst) (loop `(and . ,fms) subst))
                                                     substs))))
                 ('(and)             (list subst))
                 (`(or      . ,fms)  (append* (map (lambda (fm) (loop fm subst)) fms)))
                 (`(relate ,r ,args) (filter-not not (set-map (relation-current r)
                                                              (lambda (tuple) (unify subst args tuple)))))
                 (`(exist ,ns ,fm)   (map (lambda (subst) (shadow subst ns))
                                          (loop fm (shadow subst ns))))
                 (`(==    ,u ,v)     (let ((subst (unify subst u v)))
                                       (if subst
                                         (list subst)
                                         '()))))))))))

(define (query-run q)
  (let ((r.deps (set->list (query-dependencies q))))
    (for-each (lambda (r) (set-box! (relation-box.current r) (set))) r.deps)
    (let loop ()
      (when (foldl (lambda (r changed?)
                     (let ((current (relation-current r))
                           (next    (relation-next    r)))
                       (if (= (set-count current) (set-count next))
                         changed?
                         (begin (set-box! (relation-box.current r) next)
                                #t))))
                   #f r.deps)
        (loop)))
    (query-eval q)))

;;;;;;;;;;;;;;
;;; Syntax ;;;
;;;;;;;;;;;;;;

(define (facts vars tuples)
  `(or . ,(map (lambda (tuple) `(== ,vars ,tuple)) tuples)))

(define (== u v) `(== ,u ,v))

(define-syntax-rule
  (conde (fm00 fm0* ...) (fm0 fm* ...) ...)
  `(or (and ,fm00 ,fm0* ...) (and ,fm0 ,fm* ...) ...))
(define-syntax-rule
  (quantify type (x ...) fm0 fm ...)
  (let ((x (var 'x)) ...)
    `(type (x ...) (and ,fm0 ,fm ...))))
(define-syntax-rule
  (fresh          (x ...) fm0 fm ...)
  (quantify exist (x ...) fm0 fm ...))
(define-syntax-rule
  (query          (x ...) fm0 fm ...)
  (quantify query (x ...) fm0 fm ...))
(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) fm0 fm ...) (set->list (query-run (query (x ...) fm0 fm ...))))
    ((_ x       fm0 fm ...) (map car (run* (x) fm0 fm ...)))))
(define-syntax-rule
  (define-relation (name attr ...) fm0 fm ...)
  (define name (relation 'name '(attr ...) (box #f)
                         (lambda () (query (attr ...) fm0 fm ...)))))
