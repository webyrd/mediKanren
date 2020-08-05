#lang racket/base

(provide (all-defined-out))
(require "query.rkt")
(require  racket/list)
(require (for-syntax racket/base syntax/parse syntax/transformer))

;; different notions of consistency
;; arc: across 1-hop
;; path: across entire path
;; global: between multiple paths that might share nodes

;; global consistency
;; X -> Y
;; X -> Z
;; Y -> Z
;; need to find X -> Y -> Z
;; but imagine that X, Y, Z are sets
;; need the individual elements to be related

;; query/graph
;; returns an association list from the concept and edge names defined to their result

(define (all-consistent query-result)
  (define paths       (car query-result))
  (define named-cells (cdr query-result))
  (define kvs (map (lambda (nc) (cons (car nc) ((cdr nc) 'ref)))
                   named-cells))
  (define csets (filter (lambda (kv) (eq? (cadr kv) 'concept)) kvs))
  (define esets (filter (lambda (kv) (eq? (cadr kv) 'edge))    kvs))

  ;; symbolic: capital
  ;; concrete: lower case

  (define E=>sgs
    (foldl (lambda (path e=>s)
             (foldl (lambda (edge e=>s)
                      (define ename (cadr  edge))
                      (define sname (car   edge))
                      (hash-set e=>s ename (cddr (assoc sname csets))))
                    e=>s (path->edges path)))
           (hash) paths))
  (define E=>ogs
    (foldl (lambda (path e=>o)
             (foldl (lambda (edge e=>o)
                      (define ename (cadr  edge))
                      (define oname (caddr edge))
                      (hash-set e=>o ename (cddr (assoc oname csets))))
                    e=>o (path->edges path)))
           (hash) paths))


  (define (augment sgs ogs es)
    (map (lambda (kv)
           (define key (car kv))
           (define es  (cdr kv))
           (list key es))
         (hash->list
           (foldl (lambda (e acc)
                    (define snorm (curie-norm sgs (cadr (caddr  e))))
                    (define onorm (curie-norm ogs (cadr (cadddr e))))
                    (define key (cons snorm onorm))
                    (define existing (hash-ref acc key #f))
                    (hash-set acc key (if existing (cons e existing) (list e))))
                  (hash) es))))
  (define E=>aes
    (make-immutable-hash
      (map (lambda (kv)
             (define sgs (hash-ref E=>sgs (car kv)))
             (define ogs (hash-ref E=>ogs (car kv)))
             (define aes (augment sgs ogs (cddr kv)))
             (cons (car kv) aes))
           esets)))

  (define (C=>Es f)
    (foldl (lambda (path c=>e)
             (foldl (lambda (edge c=>e)
                      (define ename (cadr  edge))
                      (define cname (f   edge))
                      (hash-update c=>e cname
                                   (lambda (old) (cons ename old))
                                   '()))
                    c=>e (path->edges path)))
           (hash) paths))

  (define S=>Es (C=>Es car))

  (define O=>Es (C=>Es caddr))

  (define (revert-C=>Es C=>Es)
    (make-immutable-hash
      (append*
        (hash-map
          C=>Es
          (lambda (C Es)
            (map (lambda (E)
                   (cons E C))
                 Es))))))

  (define E=>S (revert-C=>Es S=>Es))
  (define E=>O (revert-C=>Es O=>Es))

  (define all-concepts (map car csets))
  (define all-edges (map car esets))

  (define (global-instances edges substitution)
    (if (null? edges)
      (list substitution)
      (let* ((E (car edges))
             (aes (hash-ref E=>aes E))
             (S (hash-ref E=>S E))
             (O (hash-ref E=>O E))
             (s (hash-ref substitution S #f))
             (o (hash-ref substitution O #f)))
        (append*
          (for/list ((ae aes))
            (let* ((snorm (caar ae))
                   (onorm (cdar ae))
                   (edge-consistent?
                     (and (or (not s) (equal? snorm s))
                          (or (not o) (equal? onorm o)))))
              (if edge-consistent?
                (let ((substitution^ (hash-set (hash-set (hash-set substitution E ae)  S snorm) O onorm)))
                  (global-instances (cdr edges)
                                    substitution^))
                '())))))))

  (global-instances all-edges (hash))
  )

(define-syntax-rule (query/graph/consistent
                      ((concept-name initial) ...)
                      ((edge-name edge-constraints ...) ...)
                      path ...)
  (let ((q (query/graph
             ((concept-name initial) ...)
             ((edge-name edge-constraints ...) ...)
             path ...)))
    (all-consistent q)))

(begin-for-syntax
 (struct selector-rep (runtime arg-count)
         #:property prop:procedure
         (lambda (self stx)
           ((make-variable-like-transformer
             (selector-rep-runtime self))
            stx)))
 (define-syntax-class edge-decl
   #:description "edge declaration"
   #:datum-literals (--)
   (pattern (subject:id -- name:id -- object:id predicate:expr)))
 (define-syntax-class concept-decl
   #:description "concept declaration"
   (pattern (name:id predicate:expr)))
 (define-syntax-class selector-id
   #:description "selector name"
   #:opaque
   (pattern _:id))
 (define-syntax-class selector-exp
   #:description "selector expression"
   #:datum-literals (rkt)
   (pattern e:id)
   (pattern (s:selector-id arg:id ...+)
            #:do [(define env-v (syntax-local-value #'s))]
            #:fail-unless (selector-rep? env-v)
            "expected a selector"
            #:fail-unless (= (selector-rep-arg-count env-v)
                             (length (syntax->list #'(arg ...))))
            "wrong number of arguments"
            #:attr e #`(#,(selector-rep-runtime env-v)
                        arg ...))
   (pattern (rkt e:expr)))
)

(define (query-runtime keys results f)
  (for/list ([result results])
            (apply f (for/list ([key keys]) (hash-ref result key)))))

(define-syntax define-selector
  (syntax-parser
   [(_ (name:id arg:id ...+) body ...+)
    (define arg-count
      (length (syntax->list #'(arg ...))))
    #`(begin
        (define (runtime arg ...) body ...)
        (define-syntax name
          (selector-rep #'runtime #,arg-count)))]))

(define-syntax query
  (syntax-parser
   #:datum-literals (select concepts edges)
   [(_ (select s:selector-exp ...+)
       (concepts c:concept-decl ...+)
       (edges e:edge-decl ...))
    #'(let ([res
             (query/graph/consistent
              ([c.name c.predicate] ...)
              ([e.name e.predicate] ...)
              [e.subject e.name e.object]
              ...)])
        (query-runtime
         '(c.name ... e.name ...)
         res
         (lambda (c.name ... e.name ...)
           (list
            s.e
            ...))))]))

(define-selector (curie x) x)

(module+
 test
 (require rackunit)
 (define q (time (query/graph/consistent
                  ((X       drug-concept?)
                   (Y       gene-or-protein)
                   (rhobtb2 "UMLS:C1425762"))
                  ((X->Y       negatively-regulates)
                   (Y->rhobtb2 positively-regulates))
                  (X X->Y Y Y->rhobtb2 rhobtb2))))

 (check-true (< 0 (length q)))
 (define q2
   (query
    (select (curie X) (curie Y))
    (concepts
     (X       drug-concept?)
     (Y       gene-or-protein)
     (rhobtb2 "UMLS:C1425762"))
    (edges
     (X -- X->Y -- Y negatively-regulates)
     (Y -- Y->rhobtb2 -- rhobtb2 positively-regulates))))
 (check-true (< 0 (length q2)))
  (define q3
   (query
    (select (rkt ((lambda (x) x) X)))
    (concepts
     (X       drug-concept?)
     (Y       gene-or-protein)
     (rhobtb2 "UMLS:C1425762"))
    (edges
     (X -- X->Y -- Y negatively-regulates)
     (Y -- Y->rhobtb2 -- rhobtb2 positively-regulates))))
  (check-true (< 0 (length q3)))
  (define q4
   (query
    (select (rkt (curie X)))
    (concepts
     (X       drug-concept?)
     (Y       gene-or-protein)
     (rhobtb2 "UMLS:C1425762"))
    (edges
     (X -- X->Y -- Y negatively-regulates)
     (Y -- Y->rhobtb2 -- rhobtb2 positively-regulates))))
 (check-true (< 0 (length q4)))
 )
