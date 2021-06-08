#lang racket/base
(provide
  define-dbk dbk dbk-parse dbk-syntax link parameter input output
  dbk-environment dbk-environment-update with-dbk-environment-update with-fresh-names
  env.empty env:new env-ref env-ref* env-set env-set* env-remove env-remove* env-bind env-bind* env-union
  literal? literal simple-parser
  parse:program parse:module parse:formula parse:term)
(require "abstract-syntax.rkt" "misc.rkt"
         racket/hash racket/list racket/match racket/set racket/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Names and parameter trees
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

(define (param-names param)
  (match param
    ((? symbol?)    (list param))
    ('()            '())
    ((cons p.a p.d) (append (param-names p.a) (param-names p.d)))
    ((? vector?)    (param-names (vector->list param)))))

(define (unique? names) (= (set-count (list->set names)) (length names)))

(define (name? x) (not (or (not x) (procedure? x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments with vocabularies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define env.empty (hash))

(define (env-ref     env vocab n)     (hash-ref (hash-ref env n (hash)) vocab #f))
(define (env-ref*    env vocab ns)    (map (lambda (n) (env-ref env vocab n)) ns))
(define (env-set     env vocab n  v)  (hash-update env n (lambda (vocab=>v) (hash-set vocab=>v vocab v)) (hash)))
(define (env-set*    env vocab ns vs) (foldl (lambda (n v env) (env-set env vocab n v)) env ns vs))

(define (env-remove  env       n)     (hash-remove env n))
(define (env-remove* env       ns)    (foldl (lambda (n e) (env-remove env n)) env ns))

(define (env-bind    env vocab n  v)  (env-set  (env-remove  env n)  vocab n  v))
(define (env-bind*   env vocab ns vs) (env-set* (env-remove* env ns) vocab ns vs))

(define (env-union   env . envs)      (foldl (lambda (e e.0)
                                               (hash-union e.0 e #:combine
                                                           (lambda (vocab=>v.0 vocab=>v)
                                                             (hash-union vocab=>v.0 vocab=>v #:combine
                                                                         (lambda (v.0 v) v)))))
                                             env envs))

(define (env:new vocab . args)
  (define nvs (plist->alist args))
  (env-set* env.empty vocab
            (map car nvs)
            (map cdr nvs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (literal? x) (or (number? x) (boolean? x) (string? x) (bytes? x)))
(define (literal  x) (if (and (number? x) (inexact? x)) (inexact->exact x) x))

(define (binding-pairs?! bps)
  (unless (and (list? bps)
               (andmap (lambda (bp) (and (list? bp)
                                         (= 2 (length bp))))
                       bps))
    (error "invalid binding pairs:" bps)))

(define ((simple-parser proc) stx)
  (cond ((list? stx) (apply proc (cdr stx)))
        (else        (error "simple-parser expects list syntax:" stx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-env (make-parameter #f))

(define (current-env-bind  vocab n  v)  (current-env (env-bind  (current-env) vocab n  v)))
(define (current-env-bind* vocab ns vs) (current-env (env-bind* (current-env) vocab ns vs)))

(define ((parse:program stx) env)
  (unless (list? stx) (error "invalid program syntax:" stx))
  (with-fresh-names
    (parameterize ((current-env env))
      (define resume (apply parse:module:begin stx))
      (define env    (current-env))
      (program:new (resume env) env))))

(define (parse:module:begin . stxs)
  (define deferred (map parse:module stxs))
  (lambda (env) (m:link (map (lambda (d) (d env)) deferred))))

(define (parse:module stx)
  (with-fresh-names
    (match stx
      ((? symbol? name)
       (define mc.b (env-ref (current-env) 'module name))
       (cond ((procedure? mc.b) (mc.b stx))
             (else              (error "unknown module clause keyword:" name mc.b))))
      ((list operator operands ...)
       (define mc.b (env-ref (current-env) 'module operator))
       (cond ((procedure? mc.b) (mc.b stx))
             (else              (error "unknown module clause operator:" operator mc.b))))
      ((? procedure? self-parse) (self-parse)))))

(define parse:module:module
  (simple-match-lambda
    ((name . body) (define resume (apply parse:module:begin body))
                   (lambda (env) (m:named name (resume env))))))

(define (quote-property property)
  (lambda (value)
    (lambda (env) (hash property value))))

(define (parse:declare-relation:projections property msg.name)
  (simple-match-lambda
    (((attrs . projections))
     (unless (unique? attrs)
       (error (string-append msg.name " attribute names must be unique:") attrs))
     (lambda (env.0)
       (define unames (map fresh-name attrs))
       (define env    (env-bind* env.0 'term attrs unames))
       (hash property (cons unames (map (lambda (proj) ((parse:term* proj) env))
                                        projections)))))))

(define parse:declare-relation:indexes (parse:declare-relation:projections 'indexes "indexes:"))
(define parse:declare-relation:modes   (parse:declare-relation:projections 'modes   "modes:"))

(define (parse:module:projections property)
  (simple-parser
    (simple-match-lambda
      (((relation . attrs) . projections)
       (parse:module:relation (cons relation attrs) property (cons attrs projections))))))

(define parse:declare-term:definition
  (lambda (body)
    (lambda (env) (hash 'definition ((parse:term body) env)))))

(define (parse:declare-relation:rule type)
  (simple-match-lambda
    (((params . formulas))
     (lambda (env)
       ;; NOTE: extracting variables in first-order positions as pattern
       ;; variables may be brittle.  It may be better to introduce a pattern
       ;; matching vocabulary to explicitly identify pattern variables.
       (define ts.params    (map (lambda (p) ((parse:term p) env)) params))
       (define names.params (set->list (t-free-vars-first-order* ts.params)))
       (define names.argument
         (map (lambda (i) (fresh-name (string->symbol (string-append "x." (number->string i)))))
              (range (length params))))
       (define formula
         ((apply parse:formula:exist names.params
                 (lambda (env)
                   (foldl f:and
                          (f:== (quote-literal #t) (quote-literal #t))
                          (map (lambda (n t) (f:== (t:var n) t))
                               names.argument
                               (t-substitute-first-order*
                                 ts.params
                                 (make-immutable-hash
                                   (map cons names.params (env-ref* env 'term names.params)))))))
                 formulas)
          env))
       (hash 'rules (list (vector type names.argument formula)))))))

(define (parse:module:rule type)
  (simple-parser
    (simple-match-lambda
      (((relation . params) . formulas) (parse:module:relation
                                          relation
                                          (parse:declare-relation:rule type)
                                          (cons params formulas))))))

(define parse:module:link
  (simple-match-lambda
    (ms (lambda (_) (m:link ms)))))

(define parse:module:parameter
  (simple-match-lambda
    (kvs (define kwargs (plist->alist kvs))
         (apply parse:module:begin
                (map (lambda (name value)
                       (lambda () (parse:module:define name (lambda (_) (quote-literal value)))))
                     (map car kwargs) (map cdr kwargs))))))

(define (parse:module:io type)
  (simple-match-lambda
    (kvs (define kwargs (plist->alist kvs))
         (apply parse:module:begin
                (map (lambda (rsig io-device)
                       (lambda () (parse:module:relation rsig type io-device)))
                     (map car kwargs) (map cdr kwargs))))))

(define parse:module:input  (parse:module:io (quote-property 'input)))
(define parse:module:output (parse:module:io (quote-property 'output)))

(define parse:module:define
  (simple-match-lambda
    (((name . params) body) (parse:module:define name (parse:term:lambda params body)))
    ((name            body) (parse:module:term   name parse:declare-term:definition body))))

(define (parse:module:declaration vocab.declare vocab.entity msg.entity m:entity)
  (simple-match-lambda
    ((name . kvs) (define kwargs (plist->alist kvs))
                  (define uname  (fresh-name name))
                  (current-env-bind vocab.entity name uname)
                  (lambda (env)
                    (define uname (env-ref env vocab.entity name))
                    (unless (name? uname)
                      (error (string-append "invalid " msg.entity " renaming:" name uname)))
                    (m:link (map (lambda (property value)
                                   (define p.b (if (procedure? property)
                                                 property
                                                 (env-ref env vocab.declare property)))
                                   (define pmap (cond ((procedure? p.b) ((p.b value) env))
                                                      (else             (hash (if p.b p.b property) value))))
                                   (m:entity uname pmap))
                                 (map car kwargs) (map cdr kwargs)))))))

(define parse:module:relation
  (simple-match-lambda
    (((relation . attrs) . kvs) (apply parse:module:relation relation (quote-property 'attributes) attrs kvs))
    (args                       (apply (parse:module:declaration 'declare-relation 'formula "relation" m:relation) args))))

(define parse:module:term              (parse:module:declaration 'declare-term     'term    "term"     m:term))

(define (parse:module:declare* parse-spec)
  (simple-match-lambda
    (specs (apply parse:module:begin
                  (map (lambda (spec)
                         (lambda () (if (list? spec)
                                      (apply parse-spec spec)
                                      (parse-spec       spec))))
                       specs)))))

(define parse:module:relations (parse:module:declare* parse:module:relation))
(define parse:module:terms     (parse:module:declare* parse:module:term))

(define parse:module:assert
  (simple-match-lambda
    ((formula) (lambda (env) (m:assert ((parse:formula formula) env))))))

(define env.initial.module.declare-relation
  (env:new
    'declare-relation
    '<<=     (parse:declare-relation:rule '<<=)
    '<<+     (parse:declare-relation:rule '<<+)
    '<<-     (parse:declare-relation:rule '<<-)
    '<<~     (parse:declare-relation:rule '<<~)
    'indexes parse:declare-relation:indexes
    'modes   parse:declare-relation:modes))

(define env.initial.module.declare-term
  (env:new
    'declare-term
    'definition parse:declare-term:definition))

(define env.initial.module.clause
  (env:new
    'module
    'module          (simple-parser parse:module:module)
    'begin           (simple-parser parse:module:begin)
    'link            (simple-parser parse:module:link)
    'relation        (simple-parser parse:module:relation)
    'relations       (simple-parser parse:module:relations)
    'term            (simple-parser parse:module:term)
    'terms           (simple-parser parse:module:terms)
    'indexes         (parse:module:projections parse:declare-relation:indexes)
    'modes           (parse:module:projections parse:declare-relation:modes)
    'parameter       (simple-parser parse:module:parameter)
    'input           (simple-parser parse:module:input)
    'output          (simple-parser parse:module:output)
    'define          (simple-parser parse:module:define)
    'assert          (simple-parser parse:module:assert)
    '<<=             (parse:module:rule '<<=)
    '<<+             (parse:module:rule '<<+)
    '<<-             (parse:module:rule '<<-)
    '<<~             (parse:module:rule '<<~)
    ;; miniKanren style module clauses
    'define-relation (parse:module:rule '<<=)))

(define env.initial.module (env-union env.initial.module.declare-relation
                                      env.initial.module.declare-term
                                      env.initial.module.clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formula parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((parse:formula stx) env)
  (with-fresh-names
    (match stx
      ((? literal? data) (f:const (literal data)))
      ((? symbol? name)
       (define f.b (env-ref env 'formula name))
       (cond ((procedure? f.b) ((f.b stx) env))
             (else             (f:const (if f.b f.b name)))))
      ((list operator operands ...)
       (define f.b (env-ref env 'formula operator))
       ((cond ((procedure? f.b) (f.b stx))
              (else             (parse:formula:relate (if f.b f.b operator) operands)))
        env))
      ((? procedure? self-parse) (self-parse env)))))

(define ((parse:formula* formulas) env)
  (map (lambda (f) ((parse:formula f) env)) formulas))

(define anonymous-vars (make-parameter #f))

(define-syntax formula/anonymous-vars
  (syntax-rules ()
    ((_ body ...) (parameterize ((anonymous-vars '()))
                    (define f (let () body ...))
                    (if (null? (anonymous-vars))
                      f
                      (f:exist (anonymous-vars) f))))))

(define ((parse:formula:relate relation operands) env)
  (formula/anonymous-vars (f:relate relation ((parse:term* operands) env))))

(define parse:formula:or
  (simple-match-lambda
    ((disjunct)             (parse:formula disjunct))
    ((disjunct . disjuncts) (lambda (env) (f:or ((parse:formula disjunct)           env)
                                                ((apply parse:formula:or disjuncts) env))))))

(define parse:formula:and
  (simple-match-lambda
    ((conjunct)             (parse:formula conjunct))
    ((conjunct . conjuncts) (lambda (env) (f:and ((parse:formula conjunct)            env)
                                                 ((apply parse:formula:and conjuncts) env))))))

(define parse:formula:not
  (simple-match-lambda ((f) (lambda (env) (f:not ((parse:formula f) env))))))

(define (parse:formula:quantifier f:quantifier msg.name)
  (simple-match-lambda
    ((names . body) (unless (unique? names)
                      (error (string-append msg.name " parameter names must be unique:") names))
                    (lambda (env)
                      (define unames (map fresh-name names))
                      (f:quantifier unames ((apply parse:formula:and body)
                                            (env-bind* env 'term names unames)))))))

(define parse:formula:exist (parse:formula:quantifier f:exist "existential quantifier"))
(define parse:formula:all   (parse:formula:quantifier f:all   "universal quantifier"))

(define parse:formula:implies
  (simple-match-lambda
    ((hypothesis conclusion) (lambda (env) (f:implies ((parse:formula hypothesis) env)
                                                      ((parse:formula conclusion) env))))))

(define parse:formula:iff
  (simple-match-lambda
    ((f.a f.b) (lambda (env) (f:iff ((parse:formula f.a) env)
                                    ((parse:formula f.b) env))))))

;; miniKanren style formulas
(define parse:formula:fresh (parse:formula:quantifier f:exist "fresh"))

(define parse:formula:conde
  (simple-match-lambda
    (clauses (apply parse:formula:or (map (lambda (conjuncts) (apply parse:formula:and conjuncts))
                                          clauses)))))

(define env.initial.formula
  (env:new
    'formula
    'or      (simple-parser parse:formula:or)
    'and     (simple-parser parse:formula:and)
    'not     (simple-parser parse:formula:not)
    'implies (simple-parser parse:formula:implies)
    'iff     (simple-parser parse:formula:iff)
    'exist   (simple-parser parse:formula:exist)
    'all     (simple-parser parse:formula:all)
    ;; miniKanren style formulas
    'fresh   (simple-parser parse:formula:fresh)
    'conde   (simple-parser parse:formula:conde)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quote-literal v) (t:quote (literal v)))

(define ((parse:term stx) env)
  (with-fresh-names
    (match stx
      ((? literal? data) (quote-literal data))
      ((? symbol?  name) ((parse:term:ref name) env))
      ((list operator operands ...)
       (define t.b (env-ref env 'term operator))
       ((cond ((procedure? t.b) (t.b stx))
              (else             (parse:term:app operator operands)))
        env))
      ((? procedure? self-parse) (self-parse env)))))

(define ((parse:term* stxs) env)
  (map (lambda (stx) ((parse:term stx) env)) stxs))

(define ((parse:term:ref name) env)
  (define t.b (env-ref env 'term name))
  (cond ((procedure? t.b) ((t.b name) env))
        (else             (t:var (if t.b t.b name)))))

(define parse:term:query
  (simple-match-lambda
    ((param-pattern . body)
     (lambda (env)
       (cond ((symbol? param-pattern)
              (define param (fresh-name param-pattern))
              (t:query param ((apply parse:formula:and body)
                              (env-bind env 'term param-pattern param))))
             (else (define param (fresh-name 'q.0))
                   (define names (param-names param-pattern))
                   (define (assign-param-pattern env)
                     (f:== param ((parse:term:simple param-pattern) env)))
                   (t:query param ((apply parse:formula:exist names assign-param-pattern body)
                                   env))))))))

(define ((parse:term:simple pattern) env)
  (let loop ((pattern pattern))
    (match pattern
      ((? symbol?)    ((parse:term:ref pattern) env))
      ('()            (quote-literal '()))
      ((cons p.a p.d) (t:cons (loop p.a) (loop p.d)))
      ((? vector?)    (t:list->vector (loop (vector->list pattern)))))))

(define parse:term:quote
  (simple-match-lambda ((value) (lambda (_) (quote-literal value)))))

(define parse:term:quasiquote
  (simple-match-lambda
    ((template)
     (lambda (env)
       (define ((keyword? k) n) (eq? k (env-ref env 'quasiquote n)))
       (define (lift tag e)     (t:cons (quote-literal tag) (t:cons e (quote-literal '()))))
       ;; NOTE: unquote-splicing support requires a safe definition of append
       (let loop ((t template) (level 0))
         (match t
           ((list (? (keyword? 'unquote)    k) e) (if (= level 0)
                                                    ((parse:term e) env)
                                                    (lift k (loop e (- level 1)))))
           ((list (? (keyword? 'quasiquote) k) t) (lift k (loop t (+ level 1))))
           ((cons t.a t.d)                        (t:cons (loop t.a level) (loop t.d level)))
           ((? vector?)                           (t:list->vector (loop (vector->list t) level)))
           ((or (? (keyword? 'quasiquote))
                (? (keyword? 'unquote)))          (error "invalid quasiquote:" t template))
           (v                                     (quote-literal v))))))))

(define parse:term:app
  (simple-match-lambda
    ((proc args) (lambda (env) (t:app ((parse:term proc)  env)
                                      ((parse:term* args) env))))))

(define parse:term:lambda
  (simple-match-lambda
    ((params body) (define names (param-names params))
                   (unless (unique? names)
                     (error "lambda parameter names must be unique:" names))
                   (lambda (env)
                     (define unames (map fresh-name names))
                     (t:lambda unames ((parse:term body)
                                       (env-bind* env 'term names unames)))))))

(define parse:term:if
  (simple-match-lambda
    ((c t f) (lambda (env) (t:if ((parse:term c) env)
                                 ((parse:term t) env)
                                 ((parse:term f) env))))))

(define parse:term:let
  (simple-match-lambda
    ((bps body) (binding-pairs?! bps)
                (parse:term:app (parse:term:lambda (map car bps) body)
                                (map cadr bps)))))

(define parse:term:letrec
  (simple-match-lambda
    ((bps body) (binding-pairs?! bps)
                (define names (param-names (map car bps)))
                (unless (unique? names)
                  (error "letrec parameter names must be unique:" names))
                (lambda (env)
                  (define unames (map fresh-name names))
                  (define rhss ((parse:term* (map cadr bps)) env))
                  (t:letrec (map cons unames rhss)
                            ((parse:term body)
                             (env-bind* env 'term names unames)))))))

(define parse:term:and
  (simple-match-lambda
    (()           (lambda (_) (quote-literal #t)))
    ((arg)        (parse:term arg))
    ((arg . args) (parse:term:if arg
                                 (apply parse:term:and args)
                                 (lambda (_) (quote-literal #f))))))

(define parse:term:or
  (simple-match-lambda
    (()           (lambda (_) (quote-literal #f)))
    ((arg)        (parse:term arg))
    ((arg . args) (lambda (env)
                    ((parse:term:let (list (list 'temp arg))
                                     (parse:term:if (parse:term:ref 'temp)
                                                    (parse:term:ref 'temp)
                                                    (lambda (_) ((apply parse:term:or args)
                                                                 env))))
                     env)))))

(define parse:term:anonymous-var
  (simple-match-lambda
    ((stx) (lambda (_)
             (unless (anonymous-vars) (error "misplaced anonymous variable:" stx))
             (define name (fresh-name '_))
             (anonymous-vars (cons name (anonymous-vars)))
             (t:var name)))))

(define (parse:term:prim name)
  (simple-match-lambda
    (((_ . args)) (lambda (env) (t:app (t:prim name) ((parse:term* args) env))))
    ((_)          (lambda (env) (t:prim name)))))

(define env.initial.term.quasiquote
  (env:new
    'quasiquote
    'quasiquote 'quasiquote
    'unquote    'unquote))

(define env.initial.term.primitive
  (apply env-union env.empty
         (map (lambda (name) (env:new 'term name (parse:term:prim name)))
              ;; TODO: some of these can be derived rather than primitive
              '(apply
                 cons car cdr
                 list->vector vector vector-ref vector-length
                 bytes-ref bytes-length bytes->string string->bytes
                 symbol->string string->symbol
                 floor + - * / =
                 equal? not
                 <= < >= >
                 any<= any< any>= any>
                 .< .<= .> .>=  ; polymorphic point-wise monotonic comparisons

                 ;; TODO: can some of these be defined relationally?
                 set set-count set-member? set-union set-intersect set-subtract
                 dict dict-count dict-ref dict-set dict-update dict-remove dict-union dict-intersect

                 min max sum length
                 map/merge map merge filter foldl foldr))))

(define env.initial.term.special
  (env:new
    'term
    '_          parse:term:anonymous-var
    'query      (simple-parser parse:term:query)
    'quote      (simple-parser parse:term:quote)
    'quasiquote (simple-parser parse:term:quasiquote)

    'if         (simple-parser parse:term:if)
    'lambda     (simple-parser parse:term:lambda)
    'let        (simple-parser parse:term:let)
    'letrec     (simple-parser parse:term:letrec)

    'and        (simple-parser parse:term:and)
    'or         (simple-parser parse:term:or)))

(define env.initial.term (env-union env.initial.term.quasiquote
                                    env.initial.term.primitive
                                    env.initial.term.special))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module macro expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: have dbk build process values (move dbk and define-dbk out of parse.rkt)
;; - built process will optionally inherit from one or more parent processes
;; - if no parent is specified, automatically use an empty process with primitive environment
;; - will no longer need dbk-environment
;; - "vertical" library/language-like linking
;;   - parent program environment(s unioned) used to initiate parsing
;;     - as opposed to "horizontal" (symmetric) linking, where environments are not involved
;;   - result will be automatically linked with parent(s)

(define dbk-environment (make-parameter (env-union env.initial.term
                                                   env.initial.formula
                                                   env.initial.module)))

(define (dbk-environment-update env->env) (dbk-environment (env->env    (dbk-environment))))

(define-syntax-rule (with-dbk-environment-update env->env body ...)
  (parameterize ((dbk-environment (env->env    (dbk-environment))))
    body ...))

;; TODO: (define-dbk name (other attributes?) (parent ...) body ...)
(define-syntax-rule (define-dbk name body ...) (define name (dbk body ...)))

;; TODO: (dbk (other attributes?) (parent ...) clauses ...) using (dbk-parse (union-of-envs-of parent ...) clauses ...)
;; dbk-parse produces AST and residual env
;; semantically process result of dbk-parse to produce a process value
(define-syntax-rule (dbk clauses ...)          (dbk-parse (dbk-syntax clauses ...)))

;; TODO: take initial environment as an argument
(define-syntax-rule (dbk-parse stx)            (with-fresh-names
                                                 ((parse:program stx) (dbk-environment))))

;; TODO: implement link as a procedure
(define-syntax link      (syntax-rules ()))
(define-syntax parameter (syntax-rules ()))
(define-syntax input     (syntax-rules ()))
(define-syntax output    (syntax-rules ()))

(define-syntax plist-syntax
  (syntax-rules ()
    ((_ key val plist ...) `(key ,val . ,(plist-syntax plist ...)))
    ((_)                   '())))

(define-syntax dbk-syntax
  (syntax-rules (module link parameter input output)
    ((_ (module    name cs ...) clauses ...) (cons `(module    ,name . ,(dbk-syntax cs  ...)) (dbk-syntax clauses ...)))
    ((_ (link      modules ...) clauses ...) (cons `(link      ,modules ...)                  (dbk-syntax clauses ...)))
    ((_ (parameter params  ...) clauses ...) (cons `(parameter . ,(plist-syntax params  ...)) (dbk-syntax clauses ...)))
    ((_ (input     inputs  ...) clauses ...) (cons `(input     . ,(plist-syntax inputs  ...)) (dbk-syntax clauses ...)))
    ((_ (output    outputs ...) clauses ...) (cons `(output    . ,(plist-syntax outputs ...)) (dbk-syntax clauses ...)))
    ((_ clause                  clauses ...) (cons 'clause                                    (dbk-syntax clauses ...)))
    ((_)                                     '())))
