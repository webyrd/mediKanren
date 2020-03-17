#lang racket/base
(provide concept/any concept/category concept/curie edge/predicate run!)
(require "synonymize.rkt"
         racket/function racket/list (except-in racket/match ==) racket/set)

;; Simple OO
(define (method-unknown name . args) (error "unknown method:" name args))
(define (method-except m names)
  (lambda (name . args)
    (apply (if (member name names) method-unknown m) name args)))
(define (method-only m names)
  (lambda (name . args)
    (apply (if (member name names) m method-unknown) name args)))
(define-syntax method-choose
  (syntax-rules (else)
    ((_ ((name ...) body ...) ... (else else-body ...))
     (lambda (method-name . args)
       (apply (case method-name
                ((name ...) body ...) ...
                (else       else-body ...))
              method-name args)))
    ((_ body ...) (method-choose body ... (else method-unknown)))))
(define-syntax method-lambda
  (syntax-rules (else)
    ((_ ((name . param) body ...) ... (else else-body ...))
     (method-choose ((name) (lambda (_ . param) body ...)) ... (else else-body ...)))
    ((_ body ...) (method-lambda body ... (else method-unknown)))))

;; Propagators
;; TODO: use a priority queue instead of a stack for better scheduling?
(define pending-propagators '())
(define (pending-add! p)
  (set! pending-propagators (cons p pending-propagators)))
(define (pending-pop!)
  (define p (car pending-propagators))
  (set! pending-propagators (cdr pending-propagators))
  (cdr p))
(define (run!) (unless (null? pending-propagators)
                 ((pending-pop!))
                 (run!)))

(define (cell =? initial-value)
  (define observers '())
  (define value     initial-value)
  (method-lambda
    ((ref)             value)
    ((set! v)          (unless (=? v value)
                         (set! value v)
                         (for-each pending-add! observers)))
    ((observer-add! p) (set! observers (cons p observers)))))

(define (propagator dependencies cost op)
  (define p (cons cost op))
  (for-each (lambda (d) (d 'observer-add! p)) dependencies)
  (pending-add! p))

;; Graph propagation
(define (string-min a b) (if (string<? a b) a b))
(define (group<? ga gb) (string<? (car ga) (car gb)))
(define (curie->group curie)
  (define synonyms (curie-synonyms (list curie)))
  (define curies (set->list synonyms))
  (define first-curie (foldl string-min (car curies) (cdr curies)))
  (define cs (find-concepts #t curies))
  (define categories (list->set (map cons (map car cs)
                                     (map (lambda (c) (cadddr (cdr c))) cs))))
  (list first-curie categories cs))

(define (concept=? ca cb)
  (match* (ca cb)
    ((`(concept . ,as) `(concept  . ,bs))
     (let loop ((as as) (bs bs))
       (or (null? as) (null? bs)
           (let ((a (car as)) (b (car bs)))
             (and (string=? (car a) (car b)) (loop (cdr as) (cdr bs)))))))
    ((_ _) (equal? ca cb))))

(define (concept-intersect ca cb)
  (if (eq? ca cb) ca
    (match* (ca cb)
      (('(any) _)      cb)
      ((_      '(any)) ca)
      ((`(concept  . ,_)    `(category . ,_)) (concept-intersect cb ca))
      ((`(category . ,cats) `(concept  . ,groups))
       (define (valid-group? g)
         (ormap (lambda (cat) (set-member? (cadr g) cat)) cats))
       `(concept . ,(filter valid-group? groups)))
      ((`(concept . ,as) `(concept  . ,bs))
       (define changed? #f)
       (define result
         (let loop ((as as) (bs bs))
           (if (or (null? as) (null? bs)) '()
             (let ((a (car as)) (b (car bs)))
               (cond ((string<? (car a) (car b))
                      (set! changed? #t) (loop (cdr as) bs))
                     ((string<? (car b) (car a))
                      (set! changed? #t) (loop as (cdr bs)))
                     (else (cons a (loop (cdr as) (cdr bs)))))))))
       (if changed? `(concept . ,result) ca)))))

(define (edge-constrain/subject e c)
  (match c
    ;; TODO: optionally find edges with only category/any constraints
    ('(any)           e)
    (`(category . ,_) e)
    (`(concept  . ,gs)
      (match e
        (`(edge . ,egs)
          (define (valid-eg? eg)
            (pair? (cdr (concept-intersect `(concept . ,(list (car eg))) c))))
          `(edge . ,(filter valid-eg? egs)))
        (`(predicate . ,ps)
          (define cs (append* (map caddr gs)))
          (define es (run* (e) (fresh (s o p db eid erest)
                                 (== e `(,db ,eid ,s ,o ,p . ,erest))
                                 (membero `(,db . ,s) cs)
                                 (membero `(,db . ,p) ps)
                                 (edgeo e))))
          `(edge . ,(map edge->edge/groups es)))))))

(define (edge-constrain/object e c)
  (match c
    ;; TODO: optionally find edges with only category/any constraints
    ('(any)           e)
    (`(category . ,_) e)
    (`(concept  . ,gs)
      (match e
        (`(edge . ,egs)
          (define (valid-eg? eg)
            (pair? (cdr (concept-intersect `(concept . ,(list (cadr eg))) c))))
          `(edge . ,(filter valid-eg? egs)))
        (`(predicate . ,ps)
          (define cs (append* (map caddr gs)))
          (define es (run* (e) (fresh (s o p db eid erest)
                                 (== e `(,db ,eid ,s ,o ,p . ,erest))
                                 (membero `(,db . ,o) cs)
                                 (membero `(,db . ,p) ps)
                                 (edgeo e)))))
          `(edge . ,(map edge->edge/groups es)))))))

(define (edge->edge/groups e) (list (curie->group (cadr (caddr e)))
                                    (curie->group (cadr (cadddr e))) e))
(define (edge-subjects e)
  (match e
    (`(edge . ,egs) (cons 'concept (sort (map car  egs) group<?)))
    (_ (list 'any))))
(define (edge-objects e)
  (match e
    (`(edge . ,egs) (cons 'concept (sort (map cadr egs) group<?)))
    (_ (list 'any))))

(define (concept-cost c)
  (match c
    (`(concept . ,gs) (foldl + 0 (map (lambda (g) (length (caddr g))) gs)))
    (_                0)))
(define (edge-cost e)
  (match e
    (`(edge . ,egs) (length egs))
    (_              0)))

(define (concept/any) (cell concept=? '(any)))
(define (concept/category categories)
  (cell concept=? (cons 'category (find-exact-categories categories))))
(define (concept/curie curie)
  (cell concept=? (cons 'concept (list (curie->group curie)))))

(define (edge/predicate predicates subject object)
  (define edge
    (cell equal? (cons 'predicate (find-exact-predicates predicates))))
  (define update-subject? #f)
  (define update-object?  #f)
  (propagator
    (list subject)
    (thunk (concept-cost (subject 'ref)))
    (thunk
      (set! update-object?  #t)
      (edge 'set! (edge-constrain/subject (edge 'ref) (subject 'ref)))))
  (propagator
    (list object)
    (thunk (concept-cost (object 'ref)))
    (thunk
      (set! update-subject? #t)
      (edge 'set! (edge-constrain/object  (edge 'ref) (object  'ref)))))
  (propagator
    (list edge)
    (thunk (edge-cost (edge 'ref)))
    (thunk
      (define e (edge 'ref))
      (when update-subject?
        (subject 'set! (concept-intersect (subject 'ref) (edge-subjects e))))
      (when update-object?
        (object  'set! (concept-intersect (object  'ref) (edge-objects  e))))
      (set! update-subject? #f)
      (set! update-object?  #f)))
  edge)
