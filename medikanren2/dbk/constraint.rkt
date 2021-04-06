#lang racket/base
(provide bis:query->stream dfs:query->stream
         relation/table define-relation/table)
(require "method.rkt" "misc.rkt" "order.rkt" "record.rkt" "stream.rkt"
         "syntax.rkt" "table.rkt" (except-in racket/match ==)
         racket/function racket/list racket/set racket/vector)

#| ;; Definitions for performance diagnostics
(require racket/pretty)
(define (pretty-var-bindings vbs)
  (map (lambda (vb) (cons (var-name (car vb)) (cdr vb))) vbs))
(define (state-var-bindings st)
  (pretty-var-bindings
    (filter (lambda (kv) (not (vcx? (cdr kv)))) (hash->list (state-var=>cx st)))))
;|#

;; TODO:
;; extra solvers, beyond bounds checking/limiting for domains and arcs:
;;   +o, *o:
;;     for generality, could use a substitute and simplify model
;;       if a + b = c, then replace c whenever it appears
;;       still need some functional dependencies (only forward?)
;;          e.g., a + b = c and a + b = d implies c = d
;;          also true that a + b = c and a + d = c implies b = d
;;          however, substituting for c and simplifying covers this:
;;            a + b = a + d, and simplifying shows b = d
;;          similar for *o, but case split with 0
;;     in special cases, can do linear programming
;;       incremental simplex
;;       keep in mind: flooro, X-lengtho, X-refo introduce integer constraints
;;     in other cases: difference equations, polynomials...
;;     calculus for optimization
;;     etc.
;;   (uninterpreted) functional dependencies:
;;     == propagation forward
;;       e.g., if (f a b = c) and (f a b = d) then (== c d)
;;     =/= propagation in reverse
;;       e.g., if (f a b = c) and (f a d = e) and (c =/= e) then (b =/= d)
;;         note: this is not a bi-implication unless the function is one-to-one
;;         (expresssed as two opposing functional dependency constraints)
;;     also can be used to encode some forms of subsumption checking
;;   X-refo: maintain minimum length and partial mapping
;;   string==byteso: look for impossible utf-8 bytes (possibly partial mapping)

;; TODO:
;; satisfiability loop for a variable constraint graph:
;;   after propagation quiessence, attempt to divide and conquer
;;     this is attempted once each time through the satisfiability loop because
;;       new assignments may lead to disconnection, allowing more decomposition
;;     decompose into subproblem per subgraph of connected variables
;;       connection comes from dependency arcs implied by shared constraints
;;       each subgraph can be satisfied independently
;;       subgraph solutions can be enumerated and composed
;;
;;   for each variable constraint subgraph:
;;     while there are unresolved variables with possible assignments:
;;       choose variable with lowest assignment-set cardinality
;;       choose an assignment for the variable
;;         may introduce/expand new constraints by stepping into a disj branch
;;       re-enter satisfiability loop with any new constraints
;;       if enumerating or loop fails, choose the next assignment
;;       if no more assignments are available, fail
;;     once no more unresolved variables, succeed
;;   compose subgraph solutions
;;     if any subgraph failed completely, composition also fails

;;; TODO: occurs check for vector-ref

(define (uid:new) (gensym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interval bounds for describing a (potentially-infinite) set of terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (term-bounds st t.0)
  (define t (walk st t.0))
  (cond ((pair? t)
         (define b.a (term-bounds st (car t)))
         (define b.d (term-bounds st (cdr t)))
         (apply bounds
                (append
                  (if (bounds-lb-inclusive? b.a)
                    (list (cons (bounds-lb b.a) (bounds-lb b.d)) (bounds-lb-inclusive? b.d))
                    (list (cons (bounds-lb b.a) term.max)        #f))
                  (if (bounds-ub-inclusive? b.a)
                    (list (cons (bounds-ub b.a) (bounds-ub b.d)) (bounds-ub-inclusive? b.d))
                    (list (cons (bounds-ub b.a) term.min)        #f)))))
        ((vector? t)
         (define b (term-bounds st (vector->list t)))
         (bounds (list->vector (bounds-lb b)) (bounds-lb-inclusive? b)
                 (list->vector (bounds-ub b)) (bounds-ub-inclusive? b)))
        ((var? t) (vcx-bounds (state-vcx-ref st t)))
        (else     (bounds t #t t #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(record vcx (bounds simple table disj use) #:transparent)
(define vcx.empty (vcx (bounds bounds.any)
                       (simple (seteq)) (table (seteq)) (disj (seteq)) (use (seteq))))

(define (vcx-bounds-set  x b) (vcx:set x (bounds b)))
(define (vcx-simple-add  x c) (vcx:set x (simple (set-add (vcx-simple x) c))))
(define (vcx-table-add   x c) (vcx:set x (table  (set-add (vcx-table  x) c))))
(define (vcx-disj-add    x c) (vcx:set x (disj   (set-add (vcx-disj   x) c))))
(define (vcx-use-add     x c) (vcx:set x (use    (set-add (vcx-use    x) c))))
(define (vcx-cx-clear x)      (vcx-bounds-set vcx.empty (vcx-bounds x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Work queue with recency-based prioritization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct queue (recent high low))
(define queue.empty (queue (seteq) '() '()))
(define (queue-empty? q) (set-empty? (queue-recent q)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Partially-satisfied state of a query's constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(record state (qterm vars log var=>cx cx pending))
(define (state:new qterm) (state (qterm qterm) (vars (term-vars qterm)) (log '())
                                 (var=>cx (hash)) (cx (hash)) (pending queue.empty)))

(define (state-vars-simplify st)
  (state:set st (vars (term-vars (walk* st (set->list (state-vars st)))))))

(define (state-vcx-ref st x)                          (hash-ref (state-var=>cx st) x vcx.empty))
(define (state-vcx-set st x t) (state:set st (var=>cx (hash-set (state-var=>cx st) x t))))

(define (state-log-add st c)
  (define log (state-log st))
  (state:set st (log (and log (cons c log)))))

(define (state-cx-add st vars vcx-add uid? c)
  (if (null? vars)
    (state-log-add st c)
    (let ((uid (or uid? (uid:new))))
      (foldl (lambda (x st)
               (state-vcx-set st x (vcx-add (state-vcx-ref st x) uid)))
             (state:set (state-log-add st c)
                        (cx (hash-set (state-cx st) uid c)))
             (set->list vars)))))

(define (state-cx-remove* st uids)
  (state:set st (cx (foldl (lambda (uid uid=>c) (hash-remove uid=>c uid))
                           (state-cx st) uids))))

(define (state-cx-update* st uids)
  (define uid=>c (state-cx st))
  (define c* (map (lambda (uid) (hash-ref uid=>c uid #f)) uids))
  (foldl/and (lambda (uid c st) (if c (c-apply st uid c) st))
             (state-cx-remove* st uids) uids c*))

(define (state-uses-empty?! st)
  (define uses (filter c:use? (hash-values (state-cx st))))
  (unless (null? uses)
    (error ":== dependencies are not ground:"
           (map (lambda (u)
                  (match-define `#s(c:use ,vs ,l ,deps ,r ,desc) u)
                  (pretty (==/use (walk* st l) (walk* st deps) r desc))
                  (error ":== dependencies are not ground:"
                         (pretty (==/use (walk* st l) (walk* st deps) r desc))))
                uses))))

(define (state-solve-lte-cycles st)
  (define cs (map (lambda (c)
                    (match-define (c:<= lhs rhs) c)
                    (c:<= (walk* st lhs) (walk* st rhs)))
                  (filter c:<=? (hash-values (state-cx st)))))
  ;; Kosaraju's algorithm for simplicity
  (define a<=b (foldl (lambda (c a<=b)
                        (match-define (c:<= lhs rhs) c)
                        (hash-update a<=b lhs (lambda (rhss) (cons rhs rhss)) '()))
                      (hash) cs))
  (define b>=a (foldl (lambda (c b>=a)
                        (match-define (c:<= lhs rhs) c)
                        (hash-update b>=a rhs (lambda (lhss) (cons lhs lhss)) '()))
                      (hash) cs))
  (define (dfs graph pending.0 order.0 source)
    (cond ((set-member? pending.0 source)
           (match-define (cons pending order)
             (foldl (lambda (target p&o)
                      (match-define (cons p o) p&o)
                      (dfs graph p o target))
                    (cons (set-remove pending.0 source) order.0)
                    (hash-ref graph source)))
           (cons pending (cons source order)))
          (else (cons pending.0 order.0))))
  (define order.<=
    (let loop ((pending (list->set (hash-keys a<=b))) (order '()))
      (if (set-empty? pending)
        order
        (match-let (((cons pending order) (dfs a<=b pending order (set-first pending))))
          (loop pending order)))))
  (define sccs
    (let loop ((order order.<=) (pending (list->set (hash-keys b>=a))))
      (if (null? order)
        '()
        (match-let (((cons pending scc) (dfs b>=a pending '() (car order))))
          (define rest (loop (cdr order) pending))
          (if (null? scc) rest (cons scc rest))))))
  (foldl/and (lambda (scc st)
               (define t.0 (car scc))
               (foldl/and (lambda (t st) (unify st t t.0)) st (cdr scc)))
             st sccs))

(define (state-schedule-add-var st x)
  (define (st/q recent high low) (state:set st (pending (queue recent high low))))
  (match-define (queue recent high low) (state-pending st))
  (cond ((or (member x high)
             (member x low))    st)
        ((set-member? recent x) (st/q          recent            high (cons x low)))
        (else                   (st/q (set-add recent x) (cons x high)        low))))

(define (state-schedule-run st)
  (define (update-and-loop st x)
    (define t (walk st x))
    (let*/and ((st (if (var? t) (var-update st t) st)))
      (state-schedule-run st)))
  (match-define (queue recent high low) (state-pending st))
  (cond ((not (null? high)) (update-and-loop
                              (state:set st (pending (queue recent (cdr high)     low)))
                              (car high)))
        ((not (null? low))  (define high.new (reverse low))
                            (update-and-loop
                              (state:set st (pending (queue recent (cdr high.new) '())))
                              (car high.new)))
        (else               (state:set st (pending queue.empty)))))

(define (state-schedule-empty? st) (queue-empty? (state-pending st)))

;; TODO: include a parameter to specify the degree of locality
(define (state-enforce-local-consistency st)
  #| ;; Performance diagnostics
  (pretty-write `(before: term: ,(walk* st (state-qterm st))
                          vcxs: ,(state-var-bindings st)))
  ;|#
  (let*/and ((st (state-schedule-run     st))
             (st (state-solve-lte-cycles st)))
    #| ;; Performance diagnostics
    (pretty-write `(after: term: ,(walk* st (state-qterm st))
                           vcxs: ,(state-var-bindings st)))
    ;|#
    (if (state-schedule-empty? st)
      st
      (state-enforce-local-consistency st))))

;; TODO: factor out table constraint satisfaction to support pre-branch global satisfiability checking
;;       in cases where we want to split a disjunction before committing to all table constraints
(define (state->satisfied-states st)
  (define (d<? d.0 d.1)
    ;; TODO: figure out a better ordering heuristic for most-constrainedness
    (< (length (cdr d.0)) (length (cdr d.1))))
  ;; TODO: how do we respect chosen search strategy, such as interleaving search?
  (define (choose-branch st.0 uid cs)
    (define (k st?) (if st? (state->satisfied-states st?) '()))
    (define st (state-cx-remove* st.0 (list uid)))
    (s-append        (k (c-apply st #f          (car cs)))
                     ;; TODO: negate (car cs) if possible, for better subsumption
              (thunk (k (c-apply st uid (c:disj (cdr cs)))))))
  (define (choose-variable st xs.observable xss)
    (define x=>c&s
      (foldl (lambda (x=>s x=>c&s)
               (foldl (lambda (x&s x=>c&s)
                        (match-define (cons x s) x&s)
                        (match (hash-ref x=>c&s x #f)
                          (#f (hash-set x=>c&s x (cons 1 s)))
                          ((cons count s.0)
                           (hash-set x=>c&s x (cons (+ count 1)
                                                    (statistics-intersect s.0 s))))))
                      x=>c&s (hash->list x=>s)))
             (hash) xss))
    ;; TODO: also consider paths provided by available table indexes, maybe via
    ;; prioritized topological sort of SCCs.
    (define (xcs<? a b)
      (match-define (cons x.a (cons count.a (statistics ratio.a card.a))) a)
      (match-define (cons x.b (cons count.b (statistics ratio.b card.b))) b)
      ;; Order by increasing size-ratio, cardinality and decreasing ref count
      ;; Prefer members of xs.observable
      (or (< ratio.a ratio.b)
          (and (= ratio.a ratio.b)
               (or (< card.a card.b)
                   (and (= card.a card.b)
                        (or (> count.a count.b)
                            (and (= count.a count.b)
                                 (set-member? xs.observable x.a)
                                 (not (set-member? xs.observable x.b)))))))))
    (define xcss   (hash->list x=>c&s))
    (define x.best (car (foldl (lambda (xcs xcs.min) (if (xcs<? xcs xcs.min) xcs xcs.min))
                               (car xcss) (cdr xcss))))

    #| ;; Performance diagnostics
    (pretty-write `(choosing: ,(var-name x.best)
                              ,(hash-ref x=>c&s x.best)
                              ,(vcx-bounds (state-vcx-ref st x.best))
                              all-choices: ,(pretty-var-bindings xcss)
                              term: ,(walk* st (state-qterm st))
                              vcxs: ,(state-var-bindings st)))
    ;(read-line)
    ;|#

    (define t (bounds-lb (vcx-bounds (state-vcx-ref st x.best))))
    (define (k st?) (if st? (state->satisfied-states st?) '()))
    (s-append        (k (var-assign    st    x.best t))
              (thunk (k (var-disassign st #f x.best t)))))
  (match (state-enforce-local-consistency st)
    (#f '())
    (st (define cxs (hash->list (state-cx st)))
        (match (map c:table-t (filter c:table? (map cdr cxs)))
          ;; If there are no more table constraints, pick a disjunction to split
          ('() (match (map (lambda (uid&c) (cons (car uid&c) (c:disj-cs (cdr uid&c))))
                           (filter (lambda (uid&c) (c:disj? (cdr uid&c))) cxs))
                 ;; If there are no more disjunctions either, we should be done
                 ('() (state-uses-empty?! st) (list st))
                 (ds  (define d.min (foldl (lambda (d d.min) (if (d<? d d.min) d d.min))
                                           (car ds) (cdr ds)))
                      (choose-branch st (car d.min) (cdr d.min)))))
          (ts (let* ((st            (state-vars-simplify st))
                     (xs.observable (state-vars          st))
                     (xss           (map (lambda (t) (table-statistics st t)) ts))
                     (sts.all       (choose-variable st xs.observable xss)))
                (if (set-empty? xs.observable) (s-limit 1 sts.all) sts.all)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal constraint algebra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct c:conj  (cs)                      #:prefab)
(struct c:disj  (cs)                      #:prefab)
(struct c:==    (l r)                     #:prefab)
(struct c:=/=   (l r)                     #:prefab)
(struct c:<=    (l r)                     #:prefab)
(struct c:use   (vars lhs args proc desc) #:prefab)
(struct c:table (t)                       #:prefab)
(struct c:proc  (proc args parents)       #:prefab)

(define (c:bounds b t)
  (match b
    ((bounds lb lbi ub ubi) (c:conj (append (list (c:<= lb t) (c:<= t ub))
                                            (if lbi '() (list (c:=/= lb t)))
                                            (if ubi '() (list (c:=/= t ub))))))
    (v                      (c:== t v))))

(define (f->c f)
  (match f
    (`#s(conj ,f1 ,f2) (define (f->cs f) (match (f->c f)
                                           ((c:conj cs) cs)
                                           (c           (list c))))
                       (c:conj (append (f->cs f1) (f->cs f2))))
    (`#s(disj ,f1 ,f2) (define (f->cs f) (match (f->c f)
                                           ((c:disj cs) cs)
                                           (c           (list c))))
                       (c:disj (append (f->cs f1) (f->cs f2))))
    (`#s(==/use ,lhs ,args ,proc ,desc) (c:use args lhs args proc desc))
    (`#s(constrain =/=    (,lhs ,rhs))  (c:=/=   lhs rhs))
    (`#s(constrain ==     (,lhs ,rhs))  (c:==    lhs rhs))
    (`#s(constrain any<=o (,lhs ,rhs))  (c:<=    lhs rhs))
    (`#s(constrain ,proc  ,args)        (c:proc  proc args (set)))))

(define (c->f c)
  (match c
    ((c:==  l r)                     (==     l r))
    ((c:=/= l r)                     (=/=    l r))
    ((c:<=  l r)                     (any<=o l r))
    ((c:disj cs)                     (apply disj* (map c->f cs)))
    ((c:conj cs)                     (apply conj* (map c->f cs)))
    ((c:table t)                     (c->f (t 'constraint)))
    ((c:proc proc args parents)      (relate proc args))
    ((c:use vars lhs args proc desc) (==/use lhs args proc desc))))

(define (c-success? c) (and (c:conj? c) (null? (c:conj-cs c))))

(define (c-simplify st.0 effort? c)
  (let*/and ((st (c-apply (state:set st.0 (log '()) (cx (hash)))
                          #f c))
             (st (if effort?
                   (let*/and ((st (foldl/and (lambda (u&c st)
                                               (match-define (cons uid? c) u&c)
                                               (c-apply st uid? c))
                                             st (hash->list (state-cx st.0)))))
                     (state-enforce-local-consistency st))
                   st)))
    (let ((log (state-log st)))
      (if (and (pair? log) (null? (cdr log)))
        (car log)
        (c:conj log)))))

(define (c-apply st uid? c)
  (match c
    ((c:==  l r)                     (unify        st      l r))
    ((c:=/= l r)                     (disunify     st uid? l r))
    ((c:<=  l r)                     (ltunify      st uid? l r))
    ((c:disj cs)                     (disjoin      st uid? cs))
    ((c:conj cs)                     (conjoin      st      cs))
    ((c:table t)                     (table-update st uid? t))
    ((c:proc proc args parents)      (proc-apply   st uid? proc args parents))
    ((c:use vars lhs args proc desc) (use          st uid? vars lhs args proc desc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unify st t1 t2)
  (let ((t1 (walk st t1)) (t2 (walk st t2)))
    (cond ((eqv? t1 t2) st)
          ((var?    t1) (var-assign st t1 t2))
          ((var?    t2) (var-assign st t2 t1))
          ((pair?   t1) (and (pair? t2)
                             (let ((st (unify st (car t1) (car t2))))
                               (and st (unify st (cdr t1) (cdr t2))))))
          ((vector? t1) (and (vector? t2) (= (vector-length t1)
                                             (vector-length t2))
                             (unify st (vector->list t1) (vector->list t2))))
          ((string? t1) (and (string? t2) (string=? t1 t2) st))
          ((bytes?  t1) (and (bytes?  t2) (bytes=?  t1 t2) st))
          (else         #f))))

(define (disunify st uid? t1 t2)
  (let ((t1 (walk st t1)) (t2 (walk st t2)))
    (cond ((eqv? t1 t2) #f)
          ((var?    t1) (var-disassign st uid? t1 t2))
          ((var?    t2) (var-disassign st uid? t2 t1))
          ((pair?   t1) (cond ((not (pair? t2)) st)
                              (else (disjoin st #f (list (c:=/= (car t1) (car t2))
                                                         (c:=/= (cdr t1) (cdr t2)))))))
          ((vector? t1) (cond ((not (and (vector? t2))
                                    (= (vector-length t1) (vector-length t2))) st)
                              (else (disjoin st #f (map c:=/=
                                                        (vector->list t1)
                                                        (vector->list t2))))))
          (else         st))))

(define (ltunify st uid? t1 t2)
  (let ((t1 (walk st t1)) (t2 (walk st t2)))
    (cond ((or (null? t1) (eq? #t t2) (eqv? t1 t2)) st)
          ((or (var? t1) (var? t2))
           (match-define (bounds lb.t1 lbi.t1 ub.t1 ubi.t1) (term-bounds st t1))
           (match-define (bounds lb.t2 lbi.t2 ub.t2 ubi.t2) (term-bounds st t2))
           (cond ((any<=?                                ub.t1 lb.t2) st)
                 (((if (and lbi.t1 ubi.t2) any<? any<=?) ub.t2 lb.t1) #f)
                 (else (let*/and
                         ((st (cond ((not (var? t1))
                                     ;; update any vars embedded in t1
                                     (let*/and ((st (ltunify st #f t1 ub.t2)))
                                       (if (not ubi.t2)
                                         (disunify st #f t1 ub.t2)
                                         st)))
                                    ((or (any<? ub.t2 ub.t1)
                                         (and ubi.t1 (not ubi.t2) (equal? ub.t2 ub.t1)))
                                     (var-assign-bounds
                                       st t1 (bounds lb.t1 lbi.t1 ub.t2 ubi.t2)))
                                    (else st)))
                          (st (cond ((not (var? t2))
                                     ;; update any vars embedded in t2
                                     (let*/and ((st (ltunify st #f lb.t1 t2)))
                                       (if (not lbi.t1)
                                         (disunify st #f lb.t1 t2)
                                         st)))
                                    ((or (any<? lb.t2 lb.t1)
                                         (and (not lbi.t1) lbi.t2 (equal? lb.t2 lb.t1)))
                                     (var-assign-bounds
                                       st t2 (bounds lb.t1 lbi.t1 ub.t2 ubi.t2)))
                                    (else st))))
                         (let* ((t1    (walk* st t1))
                                (t2    (walk* st t2))
                                (vs.t1 (term-vars t1))
                                (vs.t2 (term-vars t2))
                                (vs    (if (or (set-empty? vs.t1) (set-empty? vs.t2))
                                         '()
                                         (set->list (set-union vs.t1 vs.t2)))))
                           (state-cx-add st vs vcx-simple-add uid? (c:<= t1 t2)))))))
          ((pair? t1)
           (cond ((pair?   t2) (let*/and ((st (ltunify st #f (car t1) (car t2))))
                                 (disjoin st #f (list (c:=/= (car t1) (car t2))
                                                      (c:<=  (cdr t1) (cdr t2))))))
                 ((vector? t2)                        st)
                 (else (and (any<=? term.pair.max t2) st))))
          ((vector? t1)
           (cond ((vector? t2)
                  (define len.t1 (vector-length t1))
                  (define len.t2 (vector-length t2))
                  (cond ((< len.t1 len.t2) st)
                        ((> len.t1 len.t2) #f)
                        (else (ltunify st #f (vector->list t1) (vector->list t2)))))
                 ((pair? t2)                            #f)
                 (else (and (any<=? term.vector.max t2) st))))
          (else (and (any<=? t1 t2) st)))))

(define (disjoin st uid? cs)
  (define (c-vars c)
    (match c
      ((c:==  l r)        (set-union (term-vars l)     (term-vars r)))
      ((c:=/= l r)        (set-union (term-vars l)     (term-vars r)))
      ((c:<=  l r)        (set-union (term-vars l)     (term-vars r)))
      ((c:disj cs)        (set-union (c-vars (car cs)) (c-vars (cadr cs))))
      ((c:conj cs)        (apply set-union (seteq) (map c-vars cs)))
      ((c:use vs _ _ _ _) (term-vars vs))
      ((c:table t)        (table-vars t))
      ((c:proc  _ args _) (term-vars args))))
  (let loop ((cs cs) (cs.new '()))
    (if (null? cs)
      (match cs.new
        ('()          #f)
        ((list c.new) (c-apply st #f c.new))
        (_            (define cx (c:disj (reverse cs.new)))
                      (define vs (set->list (c-vars cx)))
                      ;; TODO: in the future when a c:proc appears in a c:disj,
                      ;; it is possible for vs to be empty even when the disj
                      ;; has not been satisfied (because the proc hasn't been
                      ;; allowed to expand yet).  Figure out how to resolve
                      ;; this, possibly by adding a queue of cxs containing
                      ;; procs that need to be expanded.
                      (if (null? vs)
                        (error "TODO: unexpanded c:proc without vars:" cx)
                        (state-cx-add st vs vcx-disj-add uid? cx))))
      (match (c-simplify st #f (car cs))
        ;; TODO: if applicable, negate (car cs) in st while simplifying (cdr cs).
        ;; This achieves some disjoint-ness across branches, reducing redundancy
        ;; in the search space.  (Ideally we would also do the same in reverse,
        ;; propagating negated constraints from later branches into earlier
        ;; branches.)
        (#f            (loop (cdr cs) cs.new))
        ((c:conj '())  st)
        ((c:disj cs.d) (loop (cdr cs) (foldl cons cs.new cs.d)))
        (c             (loop (cdr cs) (cons c cs.new)))))))

(define (conjoin st cs) (foldl/and (lambda (c st) (c-apply st #f c)) st cs))

(define (use st uid? t.vs lhs args proc desc)
  (define vars.pending (set->list (term-vars (walk* st t.vs))))
  (if (null? vars.pending)
    (unify st lhs (apply proc (walk* st args)))
    (state-cx-add st vars.pending vcx-use-add uid?
                  `#s(c:use ,vars.pending ,lhs ,args ,proc ,desc))))

(define (proc-apply st uid? proc args parents)
  ;; TODO: determine whether to expand instead of adding a c:proc
  ;; TODO: without expanding, it is possible to infinite loop when no variables
  ;; are present in args, since in that case, state-cx-add will c-apply again.
  ;; Figure out how to resolve this, possibly by adding a queue of cxs that
  ;; need to be expanded.
  (state-cx-add st (term-vars args) vcx-simple-add uid? (c:proc proc args parents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (table-update     st uid? tc) (tc 'update st uid?))
(define (table-vars               tc) (tc 'variables))
(define (table-statistics st      tc) (tc 'variable-statistics st))

(define (relation/table . pargs)
  (match-define (list info t.0) (apply materialization pargs))
  (define name             (hash-ref info 'relation-name))
  (define attribute-names  (hash-ref info 'attribute-names))
  (define primary-key-name (hash-ref info 'key-name))
  ;; TODO: this is a workaround to make sure key column is tracked for update.
  (define attrs
    (if (member primary-key-name attribute-names)
      attribute-names
      (cons #t attribute-names)))
  (define (app st args.0.0)
    ;; TODO: this introduces a phantom variable for the key column update workaround.
    ;; Is there a better solution?
    (define args.0
      (if (member primary-key-name attribute-names)
        args.0.0
        (cons (var #t) args.0.0)))
    (define (update-state st t)
      (define c=>b (t 'bounds))
      (foldl/and (lambda (c a st)
                   (c-apply st #f (c:bounds (hash-ref c=>b c bounds.any) a)))
                 st attrs args.0))
    (let*/and ((st (update-state st t.0)))
      (define args (walk* st args.0))
      (define tc
        (let controller ((t  t.0)
                         (vs (set->list (term-vars args))))
          (method-lambda
            ((constraint) (c:proc r args.0.0 (set)))
            ((variables)  vs)
            ((variable-statistics st)
             (define c=>stats (t 'statistics))
             (foldl (lambda (c a x=>stats)
                      (match (hash-ref c=>stats c #f)
                        (#f  x=>stats)
                        (s.x (define (merge s.0) (statistics-intersect s.0 s.x))
                             (foldl (lambda (x x=>stats) (hash-update x=>stats x merge s.x))
                                    x=>stats (set->list (term-vars (walk* st a)))))))
                    (hash) attrs args))
            ((update st uid?)
             (let*/and ((t (t 'update (map (lambda (c a) (cons c (term-bounds st a)))
                                           attrs args))))
               (update-state
                 (if (t 'done?)
                   st
                   ;; TODO: we only want to register with the subset of variables that are indexable
                   (let* ((vs (set->list (term-vars (walk* st vs))))
                          (tc (controller t vs)))
                     (if (null? vs)
                       (tc 'update st uid?)
                       (state-cx-add st vs vcx-table-add uid? (c:table tc)))))
                 t))))))
      (c-apply st #f (c:table tc))))
  (define r (make-relation name attribute-names))
  (relations-set! r 'definition-info info)
  (if t.0
    (relations-set! r 'apply  app)
    (relations-set! r 'expand (lambda args (== (cons name args) 'nothing))))
  r)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable-centric constraint operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (var-update st x)
  (define vcx.x (state-vcx-ref st x))
  (foldl/and (lambda (uids st) (state-cx-update* st (set->list uids)))
             (state-vcx-set st x (vcx-cx-clear vcx.x))
             (list (vcx-table  vcx.x)  ;; typically the strongest constraints
                   (vcx-simple vcx.x)
                   (vcx-disj   vcx.x)
                   (vcx-use    vcx.x))))

(define (var-assign st x t)
  ;; could replace occurs check with: (set-member? (term-vars (walk* st t)) x)
  (and (not (let occurs? ((t t))
              (cond ((pair?   t) (or (occurs? (walk st (car t)))
                                     (occurs? (walk st (cdr t)))))
                    ((vector? t) (let vloop ((i (- (vector-length t) 1)))
                                   (and (<= 0 i) (or (occurs? (walk st (vector-ref t i)))
                                                     (vloop (- i 1))))))
                    (else        (eq? x t)))))
       (let* ((vcx.x              (state-vcx-ref st x))
              (vcx.t (if (var? t) (state-vcx-ref st t) vcx.empty))
              (st    (state-log-add st (c:== x t)))
              (st    (state-vcx-set st x t)))
         (foldl/and (lambda (uids st) (state-cx-update* st (set->list uids)))
                    (c-apply st #f (c:bounds (vcx-bounds vcx.x) t))
                    (list (vcx-simple vcx.x)  ;; the least expensive constraints
                          ;; TODO: try disjs containing only simple cxs before tables.
                          ;; It may only make sense to try the disjs that only
                          ;; reference this var and no others.
                          (vcx-table  vcx.x)
                          ;; TODO: try other disjs after tables
                          (vcx-disj   vcx.x)
                          (vcx-use    vcx.x))))))

(define (var-disassign st uid? x t)
  (let* ((t    (walk* st t))
         (vs.t (term-vars t)))
    (define (add) (state-cx-add st (set->list (set-add vs.t x))
                                vcx-simple-add uid? (c:=/= x t)))
    (cond ((set-member? vs.t x) st)  ;; simple occurs check
          ((set-empty?  vs.t)
           (match-define (bounds lb lbi ub ubi) (term-bounds st x))
           (define (assign-bounds c b) (var-assign-bounds (state-log-add st c) x b))
           (cond ((and lbi (equal? t lb)) (define lb.inc (any-increment lb))
                                          (if (eq? lb lb.inc)
                                            (assign-bounds (c:=/= lb     x) (bounds lb     #f ub ubi))
                                            (assign-bounds (c:<=  lb.inc x) (bounds lb.inc #t ub ubi))))
                 ((and ubi (equal? t ub)) (define ub.dec (any-decrement ub))
                                          (if (eq? ub ub.dec)
                                            (assign-bounds (c:=/= x ub    ) (bounds lb lbi ub     #f))
                                            (assign-bounds (c:<=  x ub.dec) (bounds lb lbi ub.dec #t))))
                 ((or (any<=? t lb) (any<=? ub t)) st)
                 (else                             (add))))
          (else (match-define (bounds lb.x lbi.x ub.x ubi.x) (term-bounds st x))
                (match-define (bounds lb.t lbi.t ub.t ubi.t) (term-bounds st t))
                (if (or ((if (and lbi.t ubi.x) any<? any<=?) ub.x lb.t)
                        ((if (and ubi.t lbi.x) any<? any<=?) ub.t lb.x))
                  st
                  (add))))))

(define (var-assign-bounds st x b)
  (match-define (bounds lb lbi ub ubi) b)
  (cond ((any<?  ub lb) #f)
        ((equal? ub lb) (and lbi ubi (var-assign st x lb)))
        (else (define vcx.new (vcx-bounds-set (state-vcx-ref st x) b))
              (define st.new  (state-schedule-add-var (state-vcx-set st x vcx.new) x))
              (define domain? (and lbi ubi (finite-interval? lb ub)))
              (if domain?
                ;; TODO: instead, produce a table constraint for efficiency?
                (disjoin st.new x (map (lambda (t) (c:== x t)) domain?))
                st.new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (walk st t)
  (if (var? t)
    (let ((v=>cx (state-var=>cx st)))
      (let loop ((x t))
        (define val (hash-ref v=>cx x vcx.empty))
        (cond ((var? val) (loop val))
              ((vcx? val) x)
              (else       val))))
    t))
(define (walk* st t)
  (let loop ((term t))
    (define t (walk st term))
    (cond ((pair?   t) (cons (loop (car t)) (loop (cdr t))))
          ((vector? t) (vector-map loop t))
          (else        t))))

(define (reify st.0)
  (define (refine-bounds x st)
    (define (=/=? x t) (not (let*/and ((st (var-assign st x t)))
                              (pair? (s-force (state->satisfied-states st))))))
    (define (trim x t) (refine-bounds x (c-apply st #f (c:=/= x t))))
    (define vcx.x (state-vcx-ref st x))
    (match-define (bounds lb lbi? ub ubi?) (vcx-bounds vcx.x))
    (cond ((and lbi? (=/=? x lb)) (trim x lb))
          ((and ubi? (=/=? x ub)) (trim x ub))
          (else                   st)))
  (define term (walk* st.0 (state-qterm st.0)))
  (define xs   (set->list (term-vars term)))
  ;; Refinement should never fail after global satisfaction
  (define st   (foldl refine-bounds st.0 xs))
  (cond ((null? xs) (pretty term))
        (else (define b&uids
                (map (lambda (x)
                       (define vcx.x (state-vcx-ref st x))
                       (cons (vcx-bounds vcx.x) (vcx-simple vcx.x)))
                     xs))
              (define uids (apply set-union (seteq) (map cdr b&uids)))
              (define cxs  (walk* st (map c->f (filter-not
                                                 not (map (lambda (uid) (hash-ref (state-cx st) uid #f))
                                                          (set->list uids))))))
              (define bs
                (append* (map (lambda (x b)
                                (if (equal? b bounds.any)
                                  '()
                                  (list (list (bounds-lb b)
                                              (if (bounds-lb-inclusive? b) '<= '<)
                                              x
                                              (if (bounds-ub-inclusive? b) '<= '<)
                                              (bounds-ub b)))))
                              xs (map car b&uids))))
              (match-define (cons t c) (pretty (cons term (append bs cxs))))
              `#s(cx ,t (constraints: . ,(sort c term<?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level search strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (enumerate-and-reify st)
  (s-map (lambda (st) (reify st)) (state->satisfied-states st)))

(define (bis:query->stream q)
  (match-define `#s(query ,t ,f) q)
  (s-append* (s-map enumerate-and-reify ((bis:goal f) (state:new t)))))
(define (bis:bind s k)
  (cond ((null?      s) '())
        ((procedure? s) (thunk (bis:bind (s) k)))
        (else           (s-append/interleaving (k (car s)) (thunk (bis:bind (cdr s) k))))))
(define ((bis:apply/expand ex args) st)
  ((bis:goal (apply ex (walk* st args))) st))
(define ((bis:expand ex args) st) (thunk ((bis:goal (apply ex args)) st)))
(define (bis:goal f)
  (match f
    (`#s(conj ,f1 ,f2) (let ((k1 (bis:goal f1)) (k2 (bis:goal f2)))
                         (lambda (st) (bis:bind (k1 st) k2))))
    (`#s(disj ,f1 ,f2) (let ((k1 (bis:goal f1)) (k2 (bis:goal f2)))
                         (lambda (st) (s-append/interleaving (k1 st) (thunk (k2 st))))))
    (`#s(constrain ,(? procedure? proc) ,args)
      (define r (relations-ref proc))
      (define apply/bis    (hash-ref r 'apply/bis    #f))  ; strategy-specific application
      (define apply.r      (hash-ref r 'apply        #f))  ; strategy-agnostic application
      (define expand       (hash-ref r 'expand       #f))  ; pure expansion
      (cond (apply/bis    (apply/bis args))
            (apply.r      (lambda (st) (bis:return (apply.r st args))))
            (expand       (bis:expand       expand       args))
            (else (error "no interpretation for:" proc args))))
    (_ (define c (f->c f))
       (lambda (st) (bis:return (c-apply st #f c))))))
(define (bis:return st)         (if st (list st) '()))

(define (dfs:query->stream q)
  ((dfs:goal (query-formula q) enumerate-and-reify)
   (state:new (query-term q))))
(define ((dfs:mplus k1 k2) st) (s-append (k1 st) (thunk (k2 st))))
(define ((dfs:expand ex args k) st) ((dfs:goal (apply ex args) k) st))
(define (dfs:goal f k)
  (define loop dfs:goal)
  (match f
    (`#s(conj ,f1 ,f2) (loop f1 (loop f2 k)))
    (`#s(disj ,f1 ,f2) (dfs:mplus (loop f1 k) (loop f2 k)))
    (`#s(constrain ,(? procedure? proc) ,args)
      (define r (relations-ref proc))
      (define apply/dfs    (hash-ref r 'apply/dfs    #f))  ; strategy-specific application
      (define apply.r      (hash-ref r 'apply        #f))  ; strategy-agnostic application
      (define expand       (hash-ref r 'expand       #f))  ; pure expansion
      (cond (apply/dfs    (apply/dfs k args))
            (apply.r      (lambda (st) (dfs:return k (apply.r st args))))
            (expand       (dfs:expand       expand       args k))
            (else (error "no interpretation for:" proc args))))
    (_ (define c (f->c f))
       (lambda (st) (dfs:return k (c-apply st #f c))))))
(define (dfs:return k st)      (if st (k st) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-relation/table
  (syntax-rules ()
    ((_ name pargs ...)
     (define name (relation/table 'relation-name 'name pargs ...)))))
