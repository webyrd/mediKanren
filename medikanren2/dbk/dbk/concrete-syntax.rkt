#lang racket/base
(provide
  fresh-name with-fresh-names ??
  (for-syntax current-vocabulary)
  with-no-vocabulary with-formula-vocabulary with-term-vocabulary
  conj disj imply iff ~ all exist fresh conde query== query
  == =/= any<= any< flooro +o *o
  vector-lengtho vector-refo bytes-lengtho bytes-refo symbol==stringo string==utf8-byteso
  dbk:term dbk:app dbk:apply dbk:cons dbk:list->vector dbk:append dbk:not
  dbk:map/merge dbk:map/append dbk:map dbk:filter dbk:filter-not
  dbk:begin dbk:let dbk:let* dbk:lambda dbk:if dbk:when dbk:unless dbk:cond dbk:and dbk:or
  apply-relation
  relation-kind relation-arity relation-properties relation-properties-set!
  relation-method relation-dirty! relation-clean!
  define-relation define-relation/table define-relation/input
  define-relations)
(require "abstract-syntax.rkt" "misc.rkt" "stream.rkt"
         (for-syntax racket/base) racket/list racket/struct racket/stxparam)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fresh-name-count (make-parameter #f))

(define (call-with-fresh-names thunk)
  (if (fresh-name-count)
    (thunk)
    (parameterize ((fresh-name-count 0))
      (thunk))))

(define-syntax-rule (with-fresh-names body ...)
  (call-with-fresh-names (lambda () body ...)))

(define (fresh-name name)
  (define uid.next (fresh-name-count))
  (unless uid.next (error "fresh name not available:" name))
  (fresh-name-count (+ uid.next 1))
  (cons uid.next (if (pair? name) (cdr name) name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anonymous variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define anonymous-vars (make-parameter #f))

(define (anonymous-var (sym '_))
  (unless (anonymous-vars) (error "misplaced anonymous variable"))
  (define name (fresh-name sym))
  (anonymous-vars (cons name (anonymous-vars)))
  (t:var name))

(define-syntax formula/anonymous-vars
  (syntax-rules ()
    ((_ body ...) (parameterize ((anonymous-vars '()))
                    (define f (let () body ...))
                    (if (null? (anonymous-vars))
                      f
                      (f:exist (anonymous-vars) f))))))

(define-syntax (?? stx)
  (syntax-case stx ()
    ((_ . args) (raise-syntax-error #f "cannot apply anonymous variable" stx))
    (_          #'(anonymous-var '??))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vocabularies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-parameter stxparam.vocabulary #f)
(begin-for-syntax (define-syntax-rule (current-vocabulary)
                    (syntax-parameter-value #'stxparam.vocabulary)))

(define-syntax-rule (with-no-vocabulary      body ...) (syntax-parameterize ((stxparam.vocabulary #f))       body ...))
(define-syntax-rule (with-formula-vocabulary body ...) (syntax-parameterize ((stxparam.vocabulary 'formula)) body ...))
(define-syntax-rule (with-term-vocabulary    body ...) (syntax-parameterize ((stxparam.vocabulary 'term))    body ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (relation-apply r . args)
  (define l.args  (length args))
  (define l.extra (- (relation-arity r) (- l.args 1)))
  (when (= l.args 0)
    (error "relation-apply requires at least one argument besides the relation:" r args))
  (when (< l.extra 0)
    (error "relation-apply number of arguments exceeds relation arity:" r args))
  (define rargs      (reverse args))
  (define arg.last   (car rargs))
  (define rargs.init (cdr rargs))
  (define attrs      (hash-ref (relation-properties r) 'attributes (range (relation-arity r))))
  (define names      (s-drop (- l.args 1) attrs))
  (formula/anonymous-vars
    (let* ((vars.last (map (lambda (n) (anonymous-var (vector 'apply n))) names))
           (args.new  (foldl cons vars.last rargs.init)))
      (conj (== arg.last vars.last)
            (apply r args.new)))))

(define (relation-kind            r)     (if (relation? r)
                                           ((relation-method r) 'kind)
                                           `(primitive ,r)))
(define (relation-arity           r)     ((relation-method r) 'arity))
(define (relation-properties      r)     ((relation-method r) 'properties))
(define (relation-properties-set! r k v) ((relation-method r) 'properties-set! k v))
(define (relation-dirty!          r)     ((relation-method r) 'dirty!))
(define (relation-clean!          r)     ((relation-method r) 'clean!))

(struct relation (method)
        #:methods gen:custom-write
        ((define write-proc
           (make-constructor-style-printer
             (lambda (r) 'relation)
             (lambda (r) (list (cons 'kind  (relation-kind       r))
                               (cons 'arity (relation-arity      r))
                               (relation-properties r))))))
        #:property prop:procedure
        (lambda (r . args)
          (unless (= (relation-arity r) (length args))
            (error "relation called with invalid number of arguments" args r))
          (f:relate r (map scm->term args))))

(define (make-relation kind arity parent->self)
  (define properties (hash))
  (relation
    (parent->self
      (method-lambda
        ((kind)                   kind)
        ((arity)                  arity)
        ((properties)             properties)
        ((properties-set!    k v) (when (eq? k 'attributes)
                                    (unless (and (list? v) (= arity (length v)))
                                      (error "invalid number of attributes for arity:" arity v)))
                                  (set! properties (hash-set    properties k v)))
        ((properties-remove! k)   (set! properties (hash-remove properties k)))
        ((dirty!)                 (void))
        ((clean!)                 (void))))))

(define (relation/primitive arity name)
  (make-relation
    `(primitive ,name) arity
    (lambda (parent) parent)))

(define (relation/rule arity rule)
  (define r
    (make-relation
      'rule arity
      (lambda (parent)
        (method-lambda
          ((apply . args) (define len (length args))
                          (unless (= len arity) (error "invalid number of arguments:" arity args))
                          (apply rule args))
          ((formula)      (define attrs  (hash-ref (relation-properties r) 'attributes (range arity)))
                          (define params (map fresh-name attrs))
                          (define vars   (map t:var      params))
                          (f:all params (f:imply (apply rule vars) (apply r vars))))
          (else           parent)))))
  r)

(define (relation/table arity path)
  ;; TODO: if path is #f, use temporary storage
  (make-relation
    'table arity
    (lambda (parent)
      (method-lambda
        ;; TODO:
        ;; Define instantiation of table controllers that manage their own column constraints.
        ;; A single query might instantiate multiple controllers for the same table, each with
        ;; a different set of constraints/bounds.
        ;; Controller interface provides:
        ;; - retrieval of statistics:
        ;;   - total tuple count
        ;;   - per-column cardinality
        ;;   - per-column bounds
        ;; - update of per-column bounds
        ;; - index descriptions

        ;; TODO:
        ;; Define an interface for updating table content.
        ;; Maintain a log of insertions and deletions.
        ;; Possibly support log subscriptions to allow other processes to observe changes.
        ((path) path)
        (else   parent)))))

(define (relation/input arity produce)
  (make-relation
    'input arity
    (lambda (parent)
      (method-lambda
        ((produce) (s-enumerate 0 (produce)))
        (else      parent)))))

(define-syntax apply-relation
  (syntax-rules ()
    ((_ r . args) (formula/anonymous-vars (with-term-vocabulary (relation-apply r . args))))))

(define-syntax (define-relations stx)
  (syntax-case stx ()
    ((_ (kind (name.r param ...) body ...) ...)
     (with-syntax (((r ...) (generate-temporaries #'(name.r ...))))
       #'(begin (define-syntax (name.r stx)
                  (syntax-case stx ()
                    ((_ . args) #'(formula/anonymous-vars (with-term-vocabulary (r . args))))
                    (_          #'r))) ...
                (define-values (r ...)
                  (values (defined-relation-value (kind (name.r param ...) body ...)) ...))
                (begin (relation-properties-set! r 'name       'name.r)
                       (relation-properties-set! r 'attributes '(param ...))) ...)))))

(define-syntax defined-relation-value
  (syntax-rules (primitive rule table input)
    ((_ (rule      (name param ...) f ...))    (let ((r (relation/rule (length '(param ...))
                                                                       (lambda (param ...)
                                                                         (with-formula-vocabulary
                                                                           (conj f ...))))))
                                                 (relation-properties-set! r 'rule '((name param ...) :- f ...))
                                                 r))
    ((_ (table     (name param ...) body ...)) (relation/table     (length '(param ...)) body ...))
    ((_ (input     (name param ...) body ...)) (relation/input     (length '(param ...)) body ...))
    ((_ (primitive (name param ...)))          (relation/primitive (length '(param ...)) 'name))))

(define-syntax-rule (define-relation           body ...) (define-relations (rule      body ...)))
(define-syntax-rule (define-relation/table     body ...) (define-relations (table     body ...)))
(define-syntax-rule (define-relation/input     body ...) (define-relations (input     body ...)))
(define-syntax-rule (define-relation/primitive body ...) (define-relations (primitive body ...)))

(define-syntax-rule (define-primitive-relations signature ...) (begin (define-relation/primitive signature) ...))

(define-primitive-relations
  (==                  t1 t2)
  (=/=                 t1 t2)
  (any<=               t1 t2)
  (any<                t1 t2)
  (flooro              t1 t2)
  (+o                  t1 t2 t3)
  (*o                  t1 t2 t3)
  (vector-lengtho      t  l)
  (vector-refo         t  i  x)
  (bytes-lengtho       t  l)
  (bytes-refo          t  i  x)
  (symbol==stringo     t1 t2)
  (string==utf8-byteso t1 t2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic syntax for formulas and terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax conj*
  (syntax-rules ()
    ((_)          f:true)
    ((_ f)        f)
    ((_ fs ... f) (f:and (conj* fs ...) f))))

(define-syntax disj*
  (syntax-rules ()
    ((_)          f:false)
    ((_ f)        f)
    ((_ f fs ...) (f:or f (disj* fs ...)))))

(define-syntax-rule (conj   fs ...)    (with-formula-vocabulary (conj*   fs ...)))
(define-syntax-rule (disj   fs ...)    (with-formula-vocabulary (disj*   fs ...)))
(define-syntax-rule (imply  f.h f.c)   (with-formula-vocabulary (f:imply f.h f.c)))
(define-syntax-rule (iff    f.0 f ...) (with-formula-vocabulary (and (f:iff f.0 f) ...)))
(define-syntax-rule (~      f)         (with-formula-vocabulary (f:not   f)))

(define-syntax-rule (define-quantifier-syntax name f:quantifier)
  (... (define-syntax (name stx)
         (syntax-case stx ()
           ((_ (x ...) body ...)
            (with-syntax (((name.x ...) (generate-temporaries #'(x ...))))
              #'(let ((name.x (fresh-name 'x)) ...)
                  (let ((x (t:var name.x)) ...)
                    (f:quantifier (list name.x ...) (conj body ...))))))))))

(define-quantifier-syntax exist f:exist)
(define-quantifier-syntax all   f:all)

(define-syntax-rule (fresh (x ...) body ...) (exist (x ...) body ...))

(define-syntax-rule (conde (f.0 fs.0 ...)
                           (f   fs   ...) ...)
  (disj (conj f.0 fs.0 ...)
        (conj f   fs   ...) ...))

(define-syntax query==
  (syntax-rules ()
    ((_ result (x ...) body ...) (query== result x.0 (exist (x ...)
                                                       (== x.0 (list x ...))
                                                       body ...)))
    ((_ result x       body ...) (let* ((name.x (fresh-name 'q))
                                        (x      (t:var      name.x)))
                                   (f:query result name.x (conj body ...))))))

(define-syntax query
  (syntax-rules ()
    ((_ (x ...) body ...) (query x.0 (exist (x ...)
                                       (== x.0 (list x ...))
                                       body ...)))
    ((_ x       body ...) (with-fresh-names
                            (let* ((name.x (fresh-name 'q))
                                   (x      (t:var      name.x)))
                              (t:query name.x (conj body ...)))))))

(define (dbk:term         x)        (scm->term x))
(define (dbk:app          p . args) (t:app (scm->term p) (map scm->term args)))
(define (dbk:prim-app     p . args) (t:app (t:prim    p) (map scm->term args)))
(define (dbk:cons         a d)      (dbk:prim-app 'cons         a d))
(define (dbk:list->vector xs)       (dbk:prim-app 'list->vector xs))

(define-syntax (dbk:apply stx)
  (syntax-case stx ()
    ((_ . args) #'(dbk:prim-app 'apply . args))
    (_          #'(t:prim 'apply))))

(define-syntax-rule (define-lambda (name . params) body)
  (define-syntax (name stx)
    (syntax-case stx ()
      ((_ . args) #'(dbk:app (dbk:lambda params body) . args))
      (_          #'(dbk:lambda params body)))))

(define-lambda (dbk:not        x)                  (dbk:if x #f #t))
(define-lambda (dbk:append     xs ys)              (dbk:prim-app   'append    xs ys))
(define-lambda (dbk:map/merge  f merge default xs) (dbk:prim-app   'map/merge f merge default xs))
(define-lambda (dbk:map/append f               xs) (dbk:map/merge  f (dbk:lambda (a b) (dbk:append a b)) '()            xs))
(define-lambda (dbk:map        f               xs) (dbk:map/append (dbk:lambda (x) (list (dbk:app f x)))                xs))
(define-lambda (dbk:filter     p               xs) (dbk:map/append (dbk:lambda (x) (dbk:if (dbk:app p x) (list x) '())) xs))
(define-lambda (dbk:filter-not p               xs) (dbk:filter     (dbk:lambda (x) (dbk:not (dbk:app p x)))             xs))

(define-syntax dbk:begin
  (syntax-rules ()
    ((_)          (dbk:term (void)))
    ((_ e)        (dbk:term e))
    ((_ e es ...) (dbk:let ((temp.begin (dbk:term e))) es ...))))

(define-syntax dbk:let
  (syntax-rules ()
    ((_ ((x e) ...) body ...) (let ((name.x (fresh-name 'x)) ...)
                                (let ((x (t:var name.x)) ...)
                                  (with-term-vocabulary
                                    (t:let (list (cons name.x (dbk:term e)) ...)
                                           (dbk:begin body ...))))))))

(define-syntax dbk:let*
  (syntax-rules ()
    ((_ ()                  body ...) (dbk:let ()                              body ...))
    ((_ ((x e) (xs es) ...) body ...) (dbk:let ((x e)) (dbk:let* ((xs es) ...) body ...)))))

(define-syntax (dbk:lambda stx)
  (syntax-case stx ()
    ((_ (x ...)     body ...)
     (with-syntax (((name.x ...) (generate-temporaries #'(x ...))))
       #'(let ((name.x (fresh-name 'x)) ...)
           (let ((x (t:var name.x)) ...)
             (with-term-vocabulary
               (t:lambda (list  name.x ...)        (dbk:begin body ...)))))))
    ((_ (x ... . y) body ...)
     (with-syntax (((name.x ...) (generate-temporaries #'(x ...))))
       #'(let ((name.y (fresh-name 'y)) (name.x (fresh-name 'x)) ...)
           (let ((x (t:var name.x)) ...)
             (with-term-vocabulary
               (t:lambda (list* name.x ... name.y) (dbk:begin body ...)))))))))

(define-syntax-rule (dbk:if     c t f)      (t:if (dbk:term c) (dbk:term t) (dbk:term f)))
(define-syntax-rule (dbk:when   c body ...) (dbk:if c (dbk:begin body ...) (dbk:term (void))))
(define-syntax-rule (dbk:unless c body ...) (dbk:if c (dbk:term (void))    (dbk:begin body ...)))

(define-syntax (dbk:cond stx)
  (syntax-case stx (else =>)
    ((_)                      #'(dbk:term (void)))
    ((_ (else e ...))         #'(dbk:begin e ...))
    ((_ (else e ...) etc ...) (raise-syntax-error #f "misplaced else" stx))
    ((_ (c => p) cs ...)      #'(dbk:let ((x c)) (dbk:if x (dbk:app   p c)   (dbk:cond cs ...))))
    ((_ (c e ...) cs ...)     #'(                 dbk:if c (dbk:begin e ...) (dbk:cond cs ...)))
    ((_ c cs ...)             #'(dbk:let ((x c)) (dbk:if x x                 (dbk:cond cs ...))))))

(define-syntax dbk:and
  (syntax-rules ()
    ((_)          (dbk:term #t))
    ((_ e)        (dbk:term e))
    ((_ e es ...) (dbk:if e (dbk:and es ...) (dbk:term #f)))))

(define-syntax dbk:or
  (syntax-rules ()
    ((_)          (dbk:term #f))
    ((_ e)        (dbk:term e))
    ((_ e es ...) (dbk:let ((temp.or e)) (dbk:if temp.or temp.or (dbk:or es ...))))))
