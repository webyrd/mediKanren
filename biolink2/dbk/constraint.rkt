#lang racket/base
(provide bis:query->stream dfs:query->stream
         materialized-relation define-materialized-relation)
(require "method.rkt" "order.rkt" "stream.rkt" "syntax.rkt" "table.rkt"
         (except-in racket/match ==)
         racket/function racket/list racket/set racket/vector)

;; TODO:

;; implementation phases:
;;   pre-minimal implementation:
;;     ==: values as constraints
;;     =/=: =/=.atom, =/=.rhs, =/=*, optional subsumption checking
;;   minimal implementation must support:
;;     tables: ub, lb, lb-inclusive? ub-inclusive?, table-domains table-arcs
;;       also, subsumption and functional dependencies
;;   extended implementations may support:
;;     recursions and disjunctions as constraints: can replace =/=*
;;       recursion approximations
;;       watching 2 disjunction branches
;;     one-var any<=o constraints (for constraining type): pair/vector sub-constraints
;;     general any<=o constraints: cycle checking, pair/vector decomposition into disjunction
;;   we can support the other constraints later

;; extra solvers, beyond bounds checking/limiting for domains and arcs:
;;   table state shared between indexes
;;   any<=o: cycles become ==
;;     incremental approach:
;;       maintain a topological sort
;;       if a new any<=o is added that doesn't respect the sort, re-sort
;;         SCCs are ==
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

;; priority/event-based constraint scheduling:
;;   immediate:
;;     ==
;;   high:
;;     potentially any event (assignment, lb, ub):
;;       table relation domains
;;       disjunction domains
;;   mid or low:
;;     non-ground assignment:
;;       wait, these imply target var would be constrained, though
;;       =/=, any<=o, vector-refo, +o
;;     ground assignment:
;;       =/=
;;       may cause assignment:
;;         +o and *o
;;         X-refo, X-lengtho, symbol==stringo, string==byteso
;;         table relation unsorted arcs
;;           schedule constraints for next indexed variable
;;         disjunction unsorted arcs
;;     lb and/or ub:
;;       =/=
;;       may cause assignment:
;;         any<=o, flooro (first position)
;;         table relation sorted arcs
;;         disjunction sorted arcs
;;       does not likely cause assignment:
;;         flooro (second position)

;; satisfiability loop for a variable constraint graph:
;;   constraint propagation loop:
;;     we have high and low priority queues of updated vars
;;       initially empty first time through the satisfiability loop
;;     we have a set of new constraints produced by the program step just taken
;;       these are sorted in order of priority: immediate, high, mid, low
;;     new constraints are processed in order of priority
;;       completed/subsumed constraints are discarded, otherwise attached to
;;       appropriate variables, possibly modifying them, where any modified
;;       variable constraints are queued: high for assignments, otherwise low
;;     updated vars are then processed from job queues until quiessence
;;       if high priority queue is empty, fill with all of low priority queue
;;       if both queues are empty, we've reached quiessence
;;       otherwise, pop a var-update job off the high priority queue
;;         iterate its domain constraints to a fixed point
;;         then update the bounds of its arc constraint targets
;;           queue up any updated targets: high for assignments, otherwise low
;;       re-enter constraint propagation loop
;;     backtrack and learn clauses if conflict is detected at any point
;;
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

(define (foldl/and f acc xs)
  (let loop ((acc acc) (xs xs))
    (if (null? xs) acc
      (and acc (loop (f (car xs) acc) (cdr xs))))))
(define-syntax let*/and
  (syntax-rules ()
    ((_ () body ...) (let () body ...))
    ((_ ((lhs rhs) rest ...) body ...)
     (let ((lhs rhs)) (and lhs (let*/and (rest ...) body ...))))))

(define hasheq.empty (hash))
(define seteq.empty  (set))

;; TODO: domain trimming means explicitly testing endpoint assignments, and
;; narrowing the domain bounds when they fail (setting inclusiveness to #f,
;; or incrementing if possible).  Domain trimming should actually be
;; unnecessary given "branchless" disjunctions (those which can be tested for
;; satisfiability without branching search).  This is because =/= constraints
;; can be given the responsibility of invalidating inclusiveness.  Once such a
;; constraint simplifies to a single (=/= var value) where the value is lb or
;; ub of the var, the corresponding inclusiveness is set to #f.  And this
;; simplification will occur while satisfying branchless disjunctions.  For
;; now, even though we don't have branchless disjunctions yet and could miss
;; some invalidation opportunities by doing so, we'll still leave =/= with the
;; responsibility of invalidating inclusiveness.  This is fine because missing
;; important invalidations shouldn't be common, and when it happens it will
;; only impact performance, not soundness.  Soundness is still preserved by the
;; explicit grounding of variables by state-choose.

;; TODO: once branchless disjunctions are supported, var-update should process
;; them after updating domain cxs, but before arc cxs.  The reason for this
;; order is that domain cxs are likely the most constraining on a var's domain,
;; and branchless disjunctions that are registered to watch a var are next in
;; order of likeliness.  Arc cxs are intended to use the watched var's updated
;; bounds to constrain other vars, not to constrain the watched var itself.

;; TODO: when (bounds lb #t ub #t) describes a finite domain, introduce a
;; corresponding disjunction.  E.g., (bounds '(#t . #f) #t '#(()) #t)
;; describes the finite domain containing these 4 values:
;;   (#t . #f) (#t . #t) #() #(())
;; so if the bounds applies to the variable x, then introduce:
;;   (disj* (== x '(#t . #f)) (== x '(#t . #t)) (== x '#()) (== x '#(())))
;; This will be important for soundness once general any<=o constraints are
;; supported.  E.g., recognizing unsatisfiable disequality cliques.
;; This disjunction is analogous to a unary table constraint.  An alternative
;; to introducing an explicit disjunction is to have a set of variables that
;; are marked as having a finite domain, and have state-choose include these
;; in its working set.  Another way is to simply introduce the unary table
;; constraint.  We would also want to track that such a finite domain has
;; already been added for a variable, to prevent repeated introductions.

;; TODO: replace bounds-apply with combinations of simpler constraints.
;; applying (bounds lb lbi ub ubi) to X can be decomposed in this way:
;; (conj* (<=o lb X) (<=o X ub)
;;        (if lbi succeed (=/= lb X))
;;        (if ubi succeed (=/= ub X)))
;; and (<=o (cons lb.A lb.D) (cons t.A t.D)) can be decomposed as:
;; (conj* (<=o lb.A t.A)
;;        (disj* (=/= lb.A t.A)
;;               (<=o lb.D t.D)))
;; likewise for upper bounds
;; Also, vector comparisons can be converted to analogous list comparisons, as
;; currently done by bounds-apply.

(struct bounds (lb lb-inclusive? ub ub-inclusive?) #:prefab)
(define bounds.any (bounds term.min #t term.max #t))
(define (bounds-apply st b t (cx.rest.lb #f) (cx.rest.ub #f))
  (if (and (equal? b bounds.any) (not cx.rest.lb) (not cx.rest.ub)) st
    (let loop ((b b) (t (walk* st t)))
      (define lb  (bounds-lb b))
      (define ub  (bounds-ub b))
      (define lbi (bounds-lb-inclusive? b))
      (define ubi (bounds-ub-inclusive? b))
      (cond ((ground? t)
             (and ((if lbi any<=? any<?) lb t) ((if ubi any<=? any<?) t ub)
                  (cond ((and cx.rest.lb (equal? lb t)) (cx.rest.lb st))
                        ((and cx.rest.ub (equal? ub t)) (cx.rest.ub st))
                        (else                           st))))
            ((pair? t)
             (and (any<=? lb term.pair.max)
                  (any<=? term.pair.min ub)
                  (if (and (or (any<? lb term.pair.min)
                               (and lbi (equal? lb term.pair.min)))
                           (or (any<? term.pair.max ub)
                               (and ubi (equal? ub term.pair.max))))
                    st
                    (match-let (((cons t.a  t.d)  t)
                                ((cons lb.a lb.d) (if (any<=? term.pair.min lb)
                                                    lb term.pair.min))
                                ((cons ub.a ub.d) (if (any<=? ub term.pair.max)
                                                    ub term.pair.max))
                                (lbi.d (or lbi (any<? lb term.pair.min)))
                                (ubi.d (or ubi (any<? term.pair.max ub))))
                      ;; Assuming #t for 'a' inclusiveness is safe, but isn't
                      ;; really justified.  If 'd' is outside either of its
                      ;; bounds, the corresponding bound of 'a' will not be
                      ;; inclusive.  We could check this here, or just rely on
                      ;; a later domain trimming pass to clean up after this
                      ;; sloppiness.  On that note, domain trimming also makes
                      ;; inclusiveness tracking unnecessary, aside from any
                      ;; performance implications.
                      (define b.a (bounds lb.a #t    ub.a #t))
                      (define b.d (bounds lb.d lbi.d ub.d ubi.d))
                      (define (cx.lb st)
                        (define (cx st)
                          (bounds-apply st (bounds lb.d lbi.d term.max #t)
                                        t.d cx.rest.lb #f))
                        (bounds-apply st b.a t.a cx #f))
                      (define (cx.ub st)
                        (define (cx st)
                          (bounds-apply st (bounds term.min #t ub.d ubi.d)
                                        t.d #f cx.rest.ub))
                        (bounds-apply st b.a t.a #f cx))
                      (if (equal? lb.a ub.a)
                        (let*/and ((st.new (unify st t.a lb.a)))
                          (bounds-apply st.new b.d t.d cx.rest.lb cx.rest.ub))
                        (bounds-apply st b.a t.a cx.lb cx.ub))))))
            ((vector? t)
             (and (any<? lb term.vector.max)
                  (any<=? term.vector.min ub)
                  (let* ((len.t (vector-length t))
                         (lb.t  (make-vector len.t term.min))
                         (ub.t  (make-vector len.t term.max))
                         (lb    (if (any<? lb lb.t) lb.t lb))
                         (ub    (if (any<? ub.t ub) ub.t ub))
                         (lbi   (if (eq? lb lb.t) lbi #t))
                         (ubi   (if (eq? ub ub.t) ubi #t)))
                    (if (and (equal? lb lb.t) (equal? ub ub.t) lbi ubi) st
                      (and (any<=? lb ub.t) (any<=? lb.t ub)
                           (loop (bounds (vector->list lb) lbi
                                         (vector->list ub) ubi)
                                 (vector->list t)))))))
            (else (define vcx.old (state-var=>cx-ref st t))
                  (define b.0     (vcx-bounds vcx.old))
                  (define lb?     (any<? (bounds-lb b.0) (bounds-lb b)))
                  (define ub?     (any<? (bounds-ub b) (bounds-ub b.0)))
                  (define lbi?
                    (or lb? (and (bounds-lb-inclusive? b.0)
                                 (not (bounds-lb-inclusive? b))
                                 (equal? (bounds-lb b.0) (bounds-lb b)))))
                  (define ubi?
                    (or ub? (and (bounds-ub-inclusive? b.0)
                                 (not (bounds-ub-inclusive? b))
                                 (equal? (bounds-ub b.0) (bounds-ub b)))))
                  (let* ((lbi^ (bounds-lb-inclusive? (if lbi? b b.0)))
                         (ubi^ (bounds-ub-inclusive? (if ubi? b b.0)))
                         (lb^  (bounds-lb            (if lb?  b b.0)))
                         (ub^  (bounds-ub            (if ub?  b b.0)))
                         (vcx.old (if (and cx.rest.lb (equal? lb lb^))
                                    (vcx-arc-add vcx.old cx.rest.lb) vcx.old))
                         (vcx.old (if (and cx.rest.ub (equal? ub ub^))
                                    (vcx-arc-add vcx.old cx.rest.ub) vcx.old))
                         (st (state-var=>cx-set st t vcx.old)))
                    (if (or lb? ub? lbi? ubi?)
                      (state-vcx-update-bounds st t vcx.old
                                               (bounds lb^ lbi^ ub^ ubi^))
                      st)))))))

(define (term-bounds st term)
  (define t (walk st term))
  (cond ((pair? t)
         (define b.a (term-bounds st (car t)))
         (define b.d (term-bounds st (cdr t)))
         (define lb.a (cons (bounds-lb b.a) '()))
         (define lb.d (cons '() (bounds-lb b.d)))
         (define ub.a (cons (bounds-ub b.a) #t))
         (define ub.d (cons #t (bounds-ub b.d)))
         (match-define (cons lb lbi)
           (cond
             ((equal? lb.a lb.d) (cons lb.a (and (bounds-lb-inclusive? b.a)
                                                 (bounds-lb-inclusive? b.d))))
             ((any<? lb.a lb.d)  (cons lb.d (bounds-lb-inclusive? b.d)))
             (else               (cons lb.a (bounds-lb-inclusive? b.a)))))
         (match-define (cons ub ubi)
           (cond
             ((equal? ub.a ub.d) (cons ub.a (and (bounds-ub-inclusive? b.a)
                                                 (bounds-ub-inclusive? b.d))))
             ((any<? ub.d ub.a)  (cons ub.d (bounds-ub-inclusive? b.d)))
             (else               (cons ub.a (bounds-ub-inclusive? b.a)))))
         (bounds lb lbi ub ubi))
        ((vector? t)
         (define b (term-bounds st (vector->list t)))
         (bounds (list->vector (bounds-lb b)) (bounds-lb-inclusive? b)
                 (list->vector (bounds-ub b)) (bounds-ub-inclusive? b)))
        ((var? t) (vcx-bounds (state-var=>cx-ref st t)))
        (else     t)))

(struct vcx (bounds domain arc =/=* ==/use))
(define vcx.empty (vcx bounds.any seteq.empty seteq.empty '() '()))
(define (vcx-domain-clear x)
  (vcx (vcx-bounds x) seteq.empty (vcx-arc x) (vcx-=/=* x) (vcx-==/use x)))
(define (vcx-arc-clear x)
  (vcx (vcx-bounds x) (vcx-domain x) seteq.empty (vcx-=/=* x) (vcx-==/use x)))
(define (vcx-update x b ds as) (vcx b ds as (vcx-=/=* x) (vcx-==/use x)))
(define (vcx-bounds-set x b)  (vcx b (vcx-domain x) (vcx-arc x)
                                   (vcx-=/=* x) (vcx-==/use x)))
(define (vcx-domain-add x cx) (vcx (vcx-bounds x) (set-add (vcx-domain x) cx)
                                   (vcx-arc x) (vcx-=/=* x) (vcx-==/use x)))
(define (vcx-arc-add    x cx) (vcx (vcx-bounds x) (vcx-domain x)
                                   (set-add (vcx-arc x) cx) (vcx-=/=* x)
                                   (vcx-==/use x)))
(define (vcx-=/=*-clear x)    (vcx (vcx-bounds x) (vcx-domain x) (vcx-arc x)
                                   '() (vcx-==/use x)))
(define (vcx-=/=*-add x =/=*) (vcx (vcx-bounds x) (vcx-domain x) (vcx-arc x)
                                   (cons =/=* (vcx-=/=* x)) (vcx-==/use x)))
(define (vcx-==/use-add x u)  (vcx (vcx-bounds x) (vcx-domain x) (vcx-arc x)
                                   (vcx-=/=* x) (cons u (vcx-==/use x))))

(define (var-update st x)
  (define xcx (state-var=>cx-ref st x))
  (define b.0 (vcx-bounds xcx))
  (let*/and ((st (foldl/and cx-apply
                            (state-var=>cx-set st x (vcx-domain-clear xcx))
                            (set->list (vcx-domain xcx)))))
    (let* ((t   (walk st x))
           (xcx (if (var? t) (state-var=>cx-ref st t) vcx.empty)))
      (if (or (not (var? t)) (eq? (vcx-bounds xcx) b.0)) st
        (foldl/and cx-apply (state-var=>cx-set st t (vcx-arc-clear xcx))
                   (set->list (vcx-arc xcx)))))))

(define (cx-apply cx st) (cx st))
(define (add-domain st cx x)
  (state-vcx-update st x (lambda (vcx.old) (vcx-domain-add vcx.old cx))))
(define (add-arc st cx x)
  (state-vcx-update st x (lambda (vcx.old) (vcx-arc-add    vcx.old cx))))

(struct queue (recent high low))
(define queue.empty (queue seteq.empty '() '()))
;; tables: any finite       relations where a row    *must* be chosen
;; disjs:  any search-based relations where a branch *must* be chosen
(struct state (var=>cx store tables disjs uses pending))
(define state.empty (state hasheq.empty hasheq.empty seteq.empty seteq.empty
                           seteq.empty queue.empty))
(define (state-var=>cx-ref st x) (hash-ref (state-var=>cx st) x vcx.empty))
(define (state-var=>cx-set st x t)
  (state (hash-set (state-var=>cx st) x t) (state-store st) (state-tables st)
         (state-disjs st) (state-uses st) (state-pending st)))
(define (state-store-ref st k _) (hash-ref (state-store st) k _))
(define (state-store-set st k v)
  (state (state-var=>cx st) (hash-set (state-store st) k v) (state-tables st)
         (state-disjs st) (state-uses st) (state-pending st)))
(define (state-tables-add st t)
  (state (state-var=>cx st) (state-store st) (set-add (state-tables st) t)
         (state-disjs st) (state-uses st) (state-pending st)))
(define (state-tables-remove st t)
  (state (state-var=>cx st) (state-store st) (set-remove (state-tables st) t)
         (state-disjs st) (state-uses st) (state-pending st)))
(define (state-uses-add st u)
  (state (state-var=>cx st) (state-store st) (state-tables st) (state-disjs st)
         (set-add (state-uses st) u) (state-pending st)))
(define (state-uses-remove* st us)
  (state (state-var=>cx st) (state-store st) (state-tables st)
         (state-disjs st)
         (foldl (lambda (u us) (set-remove us u)) (state-uses st) us)
         (state-pending st)))
(define (state-uses-empty?! st)
  (unless (set-empty? (state-uses st))
    (match-define `#s(==/use ,l ,deps ,r ,desc) (set-first (state-uses st)))
    (error ":== dependencies are not ground:"
           (pretty (==/use (walk* st l) (walk* st deps) r desc)))))

(define (state-vcx-update st x update)
  (state-var=>cx-set st x (update (state-var=>cx-ref st x))))
(define (state-schedule st x)
  (define q (state-pending st))
  (if (or (member x (queue-high q)) (member x (queue-low q))) st
    (state-pending-push st x)))
(define (state-schedule-update st x vcx.new)
  (state-schedule (state-var=>cx-set st x vcx.new) x))
(define (state-vcx-update-bounds st x vcx.old b.new)
  (match-define (bounds lb lbi ub ubi) b.new)
  (cond ((any<?  ub lb) #f)
        ((equal? ub lb) (and lbi ubi (assign st x lb)))
        (else (state-schedule-update
                st x (vcx-bounds-set vcx.old b.new)))))

(define (state-pending-push st x)
  (let* ((q      (state-pending st))
         (recent (queue-recent q))
         (high   (queue-high   q))
         (low    (queue-low    q))
         (q      (if (set-member? recent x)
                   (queue recent (queue-high q) (cons x (queue-low q)))
                   (queue (set-add recent x) (cons x (queue-high q))
                          (queue-low q)))))
    (state (state-var=>cx st) (state-store st) (state-tables st)
           (state-disjs st) (state-uses st) q)))
(define (state-pending-pop st)
  (define q (state-pending st))
  (define (state/pending high low)
    (state (state-var=>cx st) (state-store st) (state-tables st)
           (state-disjs st) (state-uses st) (queue (queue-recent q) high low)))
  (define pending     (queue-high q))
  (define pending.low (queue-low  q))
  (if (null? pending)
    (if (null? pending.low) #f
      (let ((pending (reverse pending.low)))
        (cons (car pending) (state/pending (cdr pending) '()))))
    (cons (car pending) (state/pending (cdr pending) pending.low))))
(define (state-pending-clear st)
  (state (state-var=>cx st) (state-store st) (state-tables st)
         (state-disjs st) (state-uses st) queue.empty))
(define (state-pending-run st)
  (match (state-pending-pop st)
    (#f          (state-pending-clear st))
    ((cons x st) (define t (walk st x))
                 (let*/and ((st (if (var? t) (var-update st t) st)))
                   (state-pending-run st)))))

(define (state-choose st xstatss xs.observable)
  (define x=>stats
    (foldl
      (lambda (xstats x=>stats)
        (foldl (lambda (xstat x=>stats)
                 (match-define (vector x ratio.new cardinality.new) xstat)
                 (hash-update
                   x=>stats x
                   (lambda (stats)
                     (match-define (vector ratio cardinality count) stats)
                     (vector (if ratio (min ratio.new ratio) ratio.new)
                             (if cardinality (min cardinality.new cardinality)
                               cardinality.new)
                             (+ count 1)))
                   '#(#f #f 0)))
               x=>stats xstats))
      hasheq.empty xstatss))
  ;; TODO: if we don't make subsequent use of sorted xccs, just use a linear
  ;; scan to find x.best instead of sorting.
  (define (stats<? a b)
    (match-define `(,x.a . #(,ratio.a ,card.a ,count.a)) a)
    (match-define `(,x.b . #(,ratio.b ,card.b ,count.b)) b)
    ;; Sort by increasing size-ratio, cardinality and decreasing count
    ;; Prefer members of xs.observable
    (or (< ratio.a ratio.b)
        (and (= ratio.a ratio.b)
             (or (< card.a card.b)
                 (and (= card.a card.b)
                      (or (> count.a count.b)
                          (and (= count.a count.b)
                               (set-member? xs.observable x.a)
                               (not (set-member? xs.observable x.b)))))))))
  (define xccs (sort (hash->list x=>stats) stats<?))
  ;; TODO: also consider paths provided by available table indexes, maybe via
  ;; prioritized topological sort of SCCs.
  (define x.best (caar xccs))
  ;; TODO: it might be better to loop the entire state-choose.  Pruning the
  ;; domain of x.best could affect stats of other variables.
  (define v=>cx (state-var=>cx st))
  (define t (bounds-lb (vcx-bounds (state-var=>cx-ref st x.best))))
  (define st.new (assign st x.best t))
  (define (s-rest) (let ((st.skip (disunify st x.best t)))
                     (if st.skip (list st.skip) '())))
  (if st.new (cons st.new s-rest) s-rest))

(define (state-enumerate st.0 term)
  (define st (state-pending-run st.0))
  (if st
    (let* ((tcxs (set->list (state-tables st)))
           (xss  (map (lambda (tcx) (tcx-stats st tcx)) tcxs))
           (st   (foldl (lambda (tcx xstats st)
                          (if (null? xstats) (state-tables-remove st tcx) st))
                        st tcxs xss)))
      (if (set-empty? (state-tables st))
        (begin (state-uses-empty?! st) (list st))
        ;; TODO: term-vars walk* efficiency
        (let* ((xs.observable (set->list (term-vars (walk* st term))))
               (xstats        (filter pair? xss))
               (sts.all (s-append*
                          (s-map (lambda (st) (state-enumerate st xs.observable))
                                 (state-choose st xstats xs.observable)))))
          (if (null? xs.observable) (s-limit 1 sts.all) sts.all))))
    '()))

(define (assign st x t)
  (define v=>cx (state-var=>cx st))
  (and (not (occurs? st x t))
       (let* ((vcx.x                (hash-ref v=>cx x vcx.empty))
              (vcx.t   (if (var? t) (hash-ref v=>cx t vcx.empty) vcx.empty))
              (=/=**   (append (vcx-=/=* vcx.t) (vcx-=/=* vcx.x)))
              (==/use* (vcx-==/use vcx.x))
              (st      (if (eq? vcx.empty vcx.t) st
                         (state-var=>cx-set st t (vcx-=/=*-clear vcx.t))))
              (st      (state-uses-remove* st ==/use*))
              (st      (state-var=>cx-set st x t)))
         (let*/and ((st (bounds-apply st (vcx-bounds vcx.x) t))
                    (st (foldl/and cx-apply st (set->list (vcx-domain vcx.x))))
                    (st (foldl/and cx-apply st (set->list (vcx-arc    vcx.x))))
                    (st (disunify** st =/=**)))
           (use* st ==/use*)))))

(define (use st u)
  (match-define `#s(==/use ,lhs ,args ,rhs ,desc) u)
  ;; TODO: performance
  ;; * can interleave walk* and term-vars
  ;; * can stop after finding just one var
  (let* ((t  (walk* st args))
         (xs (term-vars t)))
    (if (set-empty? xs)
      (unify st lhs (apply rhs t))
      (let* ((y     (set-first xs))
             (u     (==/use lhs t rhs desc))
             (vcx.y (state-var=>cx-ref st y))
             (vcx.y (vcx-==/use-add vcx.y u))
             (st    (state-uses-add st u)))
        (state-var=>cx-set st y vcx.y)))))
(define (use* st ==/use*) (foldl/and (lambda (u st) (use st u)) st ==/use*))

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
(define (occurs? st x t)
  (let oc? ((t t))
    (cond ((pair?   t) (or (oc? (walk st (car t))) (oc? (walk st (cdr t)))))
          ((vector? t) (let vloop ((i (- (vector-length t) 1)))
                         (and (<= 0 i) (or (oc? (walk st (vector-ref t i)))
                                           (vloop (- i 1))))))
          (else        (eq? x t)))))
(define (unify st t1 t2)
  (let ((t1 (walk st t1)) (t2 (walk st t2)))
    (cond ((eqv? t1 t2) st)
          ((var?    t1) (assign st t1 t2))
          ((var?    t2) (assign st t2 t1))
          ((pair?   t1) (and (pair? t2)
                             (let ((st (unify st (car t1) (car t2))))
                               (and st (unify st (cdr t1) (cdr t2))))))
          ((vector? t1) (and (vector? t2) (= (vector-length t1)
                                             (vector-length t2))
                             (unify st (vector->list t1) (vector->list t2))))
          ((string? t1) (and (string? t2) (string=? t1 t2) st))
          ((bytes?  t1) (and (bytes?  t2) (bytes=?  t1 t2) st))
          (else         #f))))

(define (assign/log st ==* x t)
  (and (not (occurs? st x t))
       (let*/and ((st (bounds-apply st (vcx-bounds (state-var=>cx-ref st x))
                                    t)))
         (cons (state-var=>cx-set st x t)
               (cons (cons x t) ==*)))))
(define (unify/log st ==* t1 t2)
  (let ((t1 (walk st t1)) (t2 (walk st t2)))
    (cond ((eqv? t1 t2) (cons st ==*))
          ((var?    t1) (assign/log st ==* t1 t2))
          ((var?    t2) (assign/log st ==* t2 t1))
          ((pair?   t1) (and (pair? t2)
                             (let*/and ((st+ (unify/log
                                               st ==* (car t1) (car t2))))
                               (unify/log (car st+) (cdr st+)
                                          (cdr t1) (cdr t2)))))
          ((vector? t1) (and (vector? t2) (= (vector-length t1)
                                             (vector-length t2))
                             (unify/log st ==*
                                        (vector->list t1) (vector->list t2))))
          ((string? t1) (and (string? t2) (string=? t1 t2) (cons st ==*)))
          ((bytes?  t1) (and (bytes?  t2) (bytes=?  t1 t2) (cons st ==*)))
          (else         #f))))
(define (disunify* st =/=*)
  (let loop ((=/=* =/=*))
    (and (pair? =/=*)
         (let ((st+ (unify/log st '() (caar =/=*) (cdar =/=*))))
           (cond ((not st+)         st)
                 ((null? (cdr st+)) (loop (cdr =/=*)))
                 (else (define =/=*.new (append (cdr st+) (cdr =/=*)))
                       (define y        (caar =/=*.new))
                       (define vcx.y    (state-var=>cx-ref st y))
                       (define (add-=/=*)
                         (state-var=>cx-set
                           st y (vcx-=/=*-add vcx.y =/=*.new)))
                       (match =/=*.new
                         (`((,_ . ,(? ground? value)))
                           (define b   (vcx-bounds vcx.y))
                           (define lb  (bounds-lb b))
                           (define ub  (bounds-ub b))
                           (define lbi (bounds-lb-inclusive? b))
                           (define ubi (bounds-ub-inclusive? b))
                           (cond ((and lbi (equal? value lb))
                                  (define lb.inc (any-increment lb))
                                  (state-vcx-update-bounds
                                    st y vcx.y (if (eq? lb lb.inc)
                                                 (bounds lb     #f ub ubi)
                                                 (bounds lb.inc #t ub ubi))))
                                 ((and ubi (equal? value ub))
                                  (define ub.dec (any-decrement ub))
                                  (state-vcx-update-bounds
                                    st y vcx.y (if (eq? ub ub.dec)
                                                 (bounds lb lbi ub     #f)
                                                 (bounds lb lbi ub.dec #t))))
                                 (else (add-=/=*))))
                         (_ (add-=/=*)))))))))
(define (disunify** st =/=**)
  (foldl/and (lambda (=/=* st) (disunify* st =/=*)) st =/=**))
(define (disunify st t1 t2) (disunify* st (list (cons t1 t2))))

(define (reify st term)
  (define t.0 (walk* st term))
  (define xs (term-vars t.0))
  (define v=>cx (state-var=>cx st))
  (define (v->=/=* x) (vcx-=/=* (hash-ref v=>cx x vcx.empty)))
  (define =/=**.0 (append* (set-map xs v->=/=*)))
  (define (disunify*/full =/=*)
    (let loop ((=/=* =/=*) (=/=*.new '()))
      (if (null? =/=*) =/=*.new
        (let*/and ((st+ (unify/log st '() (caar =/=*) (cdar =/=*))))
          (if (null? (cdr st+)) (loop (cdr =/=*) =/=*.new)
            (let ((=/=*.0 (cdr st+)))
              ;; irrelevant variables imply irrelevant constraints
              (and (set-empty? (set-subtract (term-vars =/=*.0) xs))
                   (loop (cdr =/=*) (append =/=*.0 =/=*.new)))))))))
  (define =/=**.1 (filter-not not (map disunify*/full =/=**.0)))
  ;; pretty variables are comparable via term<?
  (match-define `(,t . ,=/=**.2) (pretty `(,t.0 . ,=/=**.1)))
  ;; normalize order of each =/= involving variables on both lhs and rhs
  (define =/=**.3
    (map (lambda (=/=*)
           (map (lambda (=/=) (sort (list (car =/=) (cdr =/=)) term<?))
                =/=*))
         =/=**.2))
  ;; eliminate subsumed =/=*s and sort final result
  (define (set-count<? a b) (< (set-count a) (set-count b)))
  (define =/=**
    (sort (map (lambda (=/=*) (sort (set->list =/=*) term<?))
               (foldl
                 (lambda (=/=* =/=**)
                   (let loop ((=/=** =/=**))
                     (if (null? =/=**) (list =/=*)
                       (let ((=/=*.0 (car =/=**)))
                         (cond ((subset? =/=*.0 =/=*) =/=**)
                               ((subset? =/=* =/=*.0) (loop (cdr =/=**)))
                               (else (cons =/=*.0 (loop (cdr =/=**)))))))))
                 '() (sort (map list->set =/=**.3) set-count<?)))
          term<?))
  ;; This whole sort-and-subset? process for subsumption is ad hoc and fragile.
  ;; TODO: define general subsumption that works with other constraints.  Save
  ;; multiple impossible variations of the current state, and check constraints
  ;; against this impossible set.  Any that fail are subsumed.  Any that do not
  ;; fail should be added to the impossibility set.  Make sure to order
  ;; constraints such that later ones wouldn't subsume those already
  ;; contributing to the impossible set.
  (if (null? =/=**) t `#s(cx ,t (=/=** . ,=/=**))))

(define (uid:new) (gensym))

(define (relation/table table-name t)
  (define (cx:new st args.0)
    (define (t-col-bounds t col) (bounds (t 'min col) #t (t 'max col) #t))
    (define (update-bounds/t col st)
      (match-define (cons col=>b t) (state-store-ref st id #f))
      (if (not (member col (t 'columns.fast))) st
        (let ((b.0 (hash-ref col=>b col bounds.any))
              (b.1 (t-col-bounds t col)))
          (if (equal? b.0 b.1) st
            (bounds-apply
              (state-store-set st id (cons (hash-set col=>b col b.1) t))
              b.1 (hash-ref col=>arg col))))))
    (define (update-bounds/arg col st)
      (let update ((st st))
        (define (add-cx st term)
          (foldl (lambda (x st) (add-domain st update x))
                 st (set->list (term-vars term))))
        (match-define (cons col=>b t) (state-store-ref st id #f))
        (define b.0  (hash-ref col=>b col bounds.any))
        (define term (walk* st (hash-ref col=>arg col)))
        (define b.t  (term-bounds st term))
        (if (bounds? b.t)
          (if (equal? b.0 b.t) (add-cx st term)
            (let*/and ((t (t (if (bounds-lb-inclusive? b.t) '>= '>)
                             col (bounds-lb b.t)))
                       (t (t (if (bounds-ub-inclusive? b.t) '<= '<)
                             col (bounds-ub b.t))))
              (let* ((b.1 (t-col-bounds t col))
                     (new (cons (hash-set col=>b col b.1) t))
                     (st  (state-store-set (add-cx st term) id new)))
                (if (equal? b.t b.1) st
                  (let ((cols.old (remove col (t 'columns.fast))))
                    (foldl/and update-bounds/t (bounds-apply st b.1 term)
                               cols.old))))))
          (let*/and ((cols.0   (t 'columns.fast))
                     (t        (t '= col b.t))
                     (cols.1   (t 'columns.fast))
                     (cols.new (set-subtract cols.1 cols.0))
                     (cols.old (set-subtract cols.1 cols.new))
                     (st       (state-store-set
                                 st id (cons (hash-remove col=>b col) t)))
                     (st       (update-bounds/new cols.new st)))
            (foldl/and update-bounds/t st cols.old)))))
    (define (update-bounds/new cols.new st)
      (match-define (cons col=>b t) (state-store-ref st id #f))
      (let* ((col=>b (foldl (lambda (c col=>b)
                              (hash-set col=>b c (t-col-bounds t c)))
                            col=>b cols.new))
             (st (state-store-set st id (cons col=>b t))))
        (foldl/and update-bounds/arg st cols.new)))
    (define id       (uid:new))
    (define args (walk* st args.0))
    (define col=>arg (make-immutable-hash (map cons (t 'columns) args)))
    (define tcx
      (method-lambda
        ((info st) (match-define (cons _ t) (state-store-ref st id #f))
                   (list table-name (t 'columns.bound) (walk* st args)))
        ((stats st)
         (match-define (cons _ t) (state-store-ref st id #f))
         (define zrat (t 'size-ratio))
         (define cols (t 'columns.fast))
         (define a*   (walk* st (map (lambda (c) (hash-ref col=>arg c)) cols)))
         (append*
           (map (lambda (c a)
                  (define cardinality (t 'max-count c))
                  (map (lambda (x) (vector x zrat cardinality))
                       (set->list (term-vars a))))
                cols a*)))))
    (let* ((st (state-store-set st id (cons (hash) t)))
           (st (state-tables-add st tcx)))
      (update-bounds/new (t 'columns.fast) st)))
  (define r (make-relation table-name (t 'columns)))
  (relations-set! r 'apply/bis
                  (lambda (args)
                    (lambda (st) (bis:return   (cx:new st args)))))
  (relations-set! r 'apply/dfs
                  (lambda (k args)
                    (lambda (st) (dfs:return k (cx:new st args)))))
  r)

(define (relation/tables relation-name attribute-names primary-key-name ts)
  (define r (make-relation relation-name attribute-names))
  (define rs
    (map (lambda (i t)
           (define name (string->symbol
                          (string-append (symbol->string relation-name) "."
                                         (number->string i))))
           (relation/table name t))
         (range (length ts)) ts))
  (define (expand . args)
    (define attr=>arg.0 (make-immutable-hash (map cons attribute-names args)))
    (define attr=>arg
      (if (member primary-key-name attribute-names) attr=>arg.0
        (hash-set attr=>arg.0 primary-key-name (var primary-key-name))))
    (apply conj*
           (map (lambda (r t)
                  (relate r (map (lambda (c)
                                   (define arg (hash-ref attr=>arg c (void)))
                                   (if (void? arg) (var '_) arg))
                                 (t 'columns))))
                rs ts)))
  (relations-set! r 'expand expand)
  r)

(define (tcx-stats st tcx) (tcx 'stats st))


;; notes from old relation.rkt

;(define (degree lb ub domain range)
;  ;; TODO: ub is #f or lb <= ub; domain and range are disjoint
;  (vector lb ub domain range))
;(define (degree-lower-bound d) (vector-ref d 0))
;(define (degree-upper-bound d) (vector-ref d 1))
;(define (degree-domain      d) (vector-ref d 2))
;(define (degree-range       d) (vector-ref d 3))

;; * extensional relation:
;;   * schema:
;;     * heading: set of attributes and their types
;;     * degree constraints (generalized functional dependencies)
;;     * possibly join and inclusion dependencies
;;   * body: finite set of tuples

;; TODO: should we interpret degree constraints to find useful special cases?
;; * functional dependency
;; * bijection (one-to-one mapping via opposing functional dependencies)
;; * uniqueness (functional dependency to full set of of attributes)

;; example degree constraints
;; TODO: are range lower bounds useful?
;'#(1 1 #(w x y z) #(pos))
;'#(1 1 #(x y z)   #(pos))  ;; even after truncating w, there are no duplicates

;; example: safe-drug -(predicate)-> gene
#|
(run* (D->G)
  (fresh (D G D-is-safe P)
    ;; D -(predicate)-> G
    (concept D 'category 'drug)   ;; probably not low cardinality          (6? optional if has-tradename already guarantees this, but is it helpful?) known D Ologn if indexed; known Ologn
    (edge D->G 'subject   D)      ;;                                       (5) 'subject range known Ologn; known D O(n) via scan
    (edge D->G 'object    G)      ;; probably lowest cardinality for D->G  (3) 'object range known Ologn; known D->G Ologn
    (edge D->G 'predicate P)      ;; probably next lowest, but unsorted    (4) known |D->G| O(|P|+logn) (would be known D->G O(nlogn)); known D->G O(nlognlog(|P|)) via scan
    (membero P predicates)        ;; P might also have low cardinality     (2) known P O(|P|)
    (== G 1234)                   ;; G should have lowest cardinality      (1) known G O1
    ;(concept G 'category 'gene)
    ;; D -(has-trade-name)-> _
    (edge D-is-safe 'subject   D) ;;                                       (7) 'subject range known Ologn; known D-is-safe Ologn
    (edge D-is-safe 'predicate 'has-tradename))) ;;                        (8) known Ologn
|#
;; about (4, should this build an intermediate result for all Ps, iterate per P, or just enumerate and filter P?):
;; * iterate        (global join on P): O(1)   space; O(|P|*(lg(edge G) + (edge P)))    time; join order is G(==), P(membero), D->G(edge, edge)
;; * intermediate    (local join on P): O(|P|) space; O(|(edge P)|lg(|P|) + lg(edge G)) time; compute D->G chunk offsets, virtual heap-sort, then join order is G(==), D->G(edge, intermediate)
;; * filter (delay consideration of P): O(1)   space; O((edge G)*lg(|P|)) time; join order is G(==), D->G(edge), P
;; (edge P) is likely larger than (edge G)
;; even if |P| is small, |(edge P)| is likely to be large
;; if |P| is small, iterate or intermediate
;; if |P| is large, iterate or filter; the larger the |P|, the better filtering becomes (negatives become less likely)
;; intermediate is rarely going to be good due to large |(edge P)|
;; iterate is rarely going to be very bad, but will repeat work for |P| > 1

;; TODO:
;; In this example query family, the query graph is tree-shaped, and the
;; smallest cardinalities will always start at leaves of the tree, so pausing
;; when cardinality spikes, and resuming at another leaf makes sense.  But what
;; happens if the smallest cardinality is at an internal node of the tree,
;; cardinality spikes, and the smallest cardinality is now along a different
;; branch/subtree?  In other words, resolving the internal node can be seen as
;; removing it, which disconnects the query graph.  The disconnected subgraphs
;; are independent, and so could be solved independently to avoid re-solving
;; each one multiple times.  This is sort of an on-the-fly tree decomposition.
;; Before fully solving each independent problem, ensure that each is
;; satisfiable.  Existential-only paths can stop after satisfiability check.


(define ((enumerate-and-reify x) st)
  (s-map (lambda (st) (reify st x)) (state-enumerate st x)))

(define (bis:query->stream q)
  (match-define `#s(query ,x ,g) q)
  (s-append* (s-map (enumerate-and-reify x) ((bis:goal g) state.empty))))
(define (bis:bind s k)
  (cond ((null?      s) '())
        ((procedure? s) (thunk (bis:bind (s) k)))
        (else           (bis:mplus (k (car s)) (thunk (bis:bind (cdr s) k))))))
(define (bis:mplus s1 s2)
  (cond ((null?      s1) (s2))
        ((procedure? s1) (thunk (bis:mplus (s2) s1)))
        (else (define d1  (cdr s1))
              (define s1^ (if (procedure? d1) d1 (thunk d1)))
              (cons (car s1) (thunk (bis:mplus (s2) s1^))))))
(define ((bis:retrieve s args) st)
  (let loop ((s (s-next s)))
    (cond ((null?      s) '())
          ((procedure? s) (thunk (loop (s))))
          (else (bis:mplus ((bis:== (car s) args) st)
                           (thunk (loop (s-next (cdr s)))))))))
(define ((bis:apply/expand ex args) st)
  ((bis:goal (apply ex (walk* st args))) st))
(define ((bis:expand ex args) st) ((bis:goal (apply ex args)) st))
(define (bis:goal g)
  (match g
    (`#s(conj ,g1 ,g2) (let ((k1 (bis:goal g1)) (k2 (bis:goal g2)))
                         (lambda (st) (bis:bind (k1 st) k2))))
    (`#s(disj ,g1 ,g2) (let ((k1 (bis:goal g1)) (k2 (bis:goal g2)))
                         (lambda (st) (bis:mplus (k1 st) (thunk (k2 st))))))
    (`#s(==/use ,_ ,_ ,_ ,_) (bis:==/use g))
    (`#s(constrain ,(? procedure? proc) ,args)
      (define r (relations-ref proc))
      (define apply/bis    (hash-ref r 'apply/bis    #f))
      (define apply/expand (hash-ref r 'apply/expand #f))  ; impure expansion
      (define expand       (hash-ref r 'expand       #f))  ; pure expansion
      (cond (apply/bis    (apply/bis args))
            (apply/expand (bis:apply/expand apply/expand args))
            (expand       (bis:expand       expand       args))
            (else (error "no interpretation for:" proc args))))
    (`#s(constrain (retrieve ,s) ,args)     (bis:retrieve s args))
    (`#s(constrain ==            (,t1 ,t2)) (bis:==  t1 t2))
    (`#s(constrain =/=           (,t1 ,t2)) (bis:=/= t1 t2))))
(define (bis:return st)      (if st (list st) '()))
(define ((bis:== t1 t2)  st) (bis:return (unify st t1 t2)))
(define ((bis:==/use u)  st) (bis:return (use st u)))
(define ((bis:=/= t1 t2) st) (bis:return (disunify st t1 t2)))

(define (dfs:query->stream q) ((dfs:query q) state.empty))
(define (dfs:query q)
  (match-define `#s(query ,x ,g) q)
  (dfs:goal g (enumerate-and-reify x)))
(define ((dfs:mplus k1 k2) st) (s-append (k1 st) (thunk (k2 st))))
(define ((dfs:retrieve s args k) st)
  (let loop ((s (s-next s)))
    (cond ((null?      s) '())
          ((procedure? s) (thunk (loop (s))))
          (else ((dfs:mplus (dfs:==       (car s) args k)
                            (dfs:retrieve (cdr s) args k))
                 st)))))
(define ((dfs:apply/expand ex args k) st)
  ((dfs:goal (apply ex (walk* st args)) k) st))
(define ((dfs:expand ex args k) st) ((dfs:goal (apply ex args) k) st))
(define (dfs:goal g k)
  (define loop dfs:goal)
  (match g
    (`#s(conj ,g1 ,g2) (loop g1 (loop g2 k)))
    (`#s(disj ,g1 ,g2) (dfs:mplus (loop g1 k) (loop g2 k)))
    (`#s(==/use ,_ ,_ ,_ ,_) (dfs:==/use g k))
    (`#s(constrain ,(? procedure? proc) ,args)
      (define r (relations-ref proc))
      (define apply/dfs    (hash-ref r 'apply/dfs    #f))
      (define apply/expand (hash-ref r 'apply/expand #f))  ; impure expansion
      (define expand       (hash-ref r 'expand       #f))  ; pure expansion
      (cond (apply/dfs    (apply/dfs k args))
            (apply/expand (dfs:apply/expand apply/expand args k))
            (expand       (dfs:expand       expand       args k))
            (else (error "no interpretation for:" proc args))))
    (`#s(constrain (retrieve ,s) ,args)     (dfs:retrieve s args k))
    (`#s(constrain ==            (,t1 ,t2)) (dfs:==  t1 t2 k))
    (`#s(constrain =/=           (,t1 ,t2)) (dfs:=/= t1 t2 k))))
(define (dfs:return k st)      (if st (k st) '()))
(define ((dfs:== t1 t2 k)  st) (dfs:return k (unify st t1 t2)))
(define ((dfs:==/use u k)  st) (dfs:return k (use st u)))
(define ((dfs:=/= t1 t2 k) st) (dfs:return k (disunify st t1 t2)))

(define (materialized-relation kwargs)
  (match-define (list name attribute-names primary-key-name ts)
    (materialization kwargs))
  (relation/tables name attribute-names primary-key-name ts))

(define-syntax define-materialized-relation
  (syntax-rules ()
    ((_ name kwargs) (define name (materialized-relation
                                    `((relation-name . name) . ,kwargs))))))
