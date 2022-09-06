#lang racket/base
(provide simplify-program factor-program)
(require racket/list racket/match racket/set)

;; TODO: lift quantifiers to top of relation definition (or query) formula

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Program parts
;(define (R Ns) Fs)
;(query  Ns     Fs)

;; Formulas F
;(relate R Ts)
;(not    F)
;(and    Fs)
;(or     Fs)
;(imply  F F)
;(iff    Fs)
;(exist  Ns F)
;(all    Ns F)

;; Terms T
;(quote C)
;(var   N)
;(app   Func Ts)

(define (quote?      t) (eq? (car t) 'quote))
(define (quote-value t) (cadr t))

(define (var      n) `(var ,n))
(define (var?     t) (eq? (car t) 'var))
(define (var-name t) (cadr t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simplification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - classical transformations:
;;   - `(not (not F))  ==> F`
;;   - `(iff P Q)      ==> (or (and P Q) (and (not P) (not Q)))`
;;   - `(imply P Q)    ==> (not (and P (not Q)))`
;;   - `(all X F)      ==> (not (exist X (not F)))`

;; - constructive/minimal transformations:
;;   - `(not (not (not F))) ==> (not F)`
;;   - `(not (or P Q))      ==> (and (not P) (not Q))`
;;   - `(iff P Q)           ==> (and (imply P Q) (imply Q P))`
;;   - `(imply P (not Q))   ==> (not (and P Q))`
;;   - `(all X (not F))     ==> (not (exist X F))`

;; - primitive transformations:
;;   - `(not (==    A B)) ==> (=/=   A B)`
;;   - `(not (=/=   A B)) ==> (==    A B)`
;;   - `(not (any<= A B)) ==> (any<  B A)`
;;   - `(not (any<  A B)) ==> (any<= B A)`

(define (simplify-program full? parts)
  (map (lambda (part)
         (match part
           (`(define (,r . ,params) ,f) `(define (,r . ,params) ,(simplify-formula full? f)))
           (`(query  ,params        ,f) `(query  ,params        ,(simplify-formula full? f)))))
       parts))

(define (simplify-formula full? formula)  ; currently applying classical transformations
  (define (loop/not f)
    (match f
      (`(relate      ,r ,@ts)    (let ((ts (map simplify-term ts)))
                                   (define (k r ts) `(relate ,r . ,ts))
                                   (match r
                                     ('==           (k '=/=            ts))
                                     ('=/=          (k '==             ts))
                                     ('any<=        (k 'any<  (reverse ts)))
                                     ('any<         (k 'any<= (reverse ts)))
                                     (_      `(not ,(k r               ts))))))
      (`(all         ,params ,f)      `(exist ,params ,(loop/not f)))
      (`(exist       ,params ,f) `(not (exist ,params ,(loop     f))))
      (`(not         ,f)         (loop f))
      (`(and         ,@fs)       `(not (and . ,(map loop fs))))
      (`(or          ,@fs)       `(and . ,(map loop/not fs)))
      (`(iff         ,@fs)       (if full? ; defer iff transformation until after factoring, for efficiency
                                   (loop/not `(or (and . ,fs)
                                                  (and . ,(map (lambda (f) `(not ,f)) fs))))
                                   `(not (iff . ,(map loop fs)))))
      (`(imply       ,p ,q)      (loop/not `(not (and ,p (not ,q)))))))
  (define (loop f)
    (match f
      (`(relate      ,r ,@ts)    `(relate ,r . ,(map simplify-term ts)))
      (`(all         ,params ,f) `(not (exist ,params ,(loop/not f))))
      (`(exist       ,params ,f)      `(exist ,params ,(loop     f)))
      (`(not         ,f)         (loop/not f))
      (`(and         ,@fs)       `(and . ,(map loop fs)))
      (`(or          ,@fs)       `(or  . ,(map loop fs)))
      (`(iff         ,@fs)       (if full? ; defer iff transformation until after factoring, for efficiency
                                   (loop `(or (and . ,fs)
                                              (and . ,(map (lambda (f) `(not ,f)) fs))))
                                   `(iff . ,(map loop fs))))
      (`(imply       ,p ,q)      (loop `(not (and ,p (not ,q)))))))
  (loop formula))

(define (simplify-term term)
  (match term
    (`(app ,func ,@ts)
      (match (cons func (map simplify-term ts))
        (`(cons         (quote ,a) (quote ,d))                                  `(quote ,(cons a d)))
        (`(vector       . ,(? (lambda (ts) (andmap quote? ts)) ts))             `(quote ,(list->vector (map quote-value ts))))
        (`(list->vector ,(? quote? (? (lambda (t) (list? (quote-value t))) t))) `(quote ,(list->vector (quote-value t))))
        (`(,_           . ,ts) `(app ,func . ,ts))))
    (_ term)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program factoring via definition introduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (factor-program parts)
  (factor-locally
    (define factored (map (lambda (part)
                            (match part
                              (`(define (,r . ,params) ,f) `(define (,r . ,params) ,(factor-formula f)))
                              (`(query  ,params        ,f) `(query  ,params                   ,(factor-formula f)))))
                          parts))
    (define new      (map (lambda (f&r)
                            (match-define (cons f `(relate ,r . ,vs)) f&r)
                            `(define (,r . ,(map (lambda (v) (match-define `(var ,name) v) name) vs)) ,f))
                          (sort (hash->list (formula=>relate))
                                (lambda (kv.a kv.b)
                                  (define (kv-r kv) (cadr (caddr kv)))
                                  (< (kv-r kv.a) (kv-r kv.b))))))
    (append new factored)))

(define (factor-formula formula)
  (define (replace f) (rename-locally
                        (formula-unrename-variables (formula->relate (formula-rename-variables f)))))
  (match formula
    (`(relate                             ,r ,@ts)    (define f.new `(relate ,r . ,ts))
                                                      (if (foldl (lambda (t seen) (and seen (var? t) (not (set-member? seen (var-name t)))
                                                                                       (set-add seen (var-name t))))
                                                                 (set) ts)
                                                        f.new
                                                        (replace f.new)))
    (`(,(and (or 'exist 'all) quantifier) ,params ,f) (replace `(,quantifier ,params ,(factor-formula f))))
    (`(,connective                        ,@fs)       (replace `(,connective . ,(map factor-formula fs))))))

(define-syntax-rule (factor-locally body ...) (parameterize ((formula=>relate (hash))) body ...))

(define formula=>relate (make-parameter #f))

(define (formula->relate f)
  (define f=>r (formula=>relate))
  (or (hash-ref f=>r f #f)
      (let* ((count (hash-count f=>r))
             (r     `(relate (new ,count) . ,(map var (remove-duplicates (formula-free-names f))))))
        (formula=>relate (hash-set f=>r f r))
        r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (term-free-names t)
  (match t
    (`(quote ,_)         '())
    (`(var   ,name)      (list name))
    (`(app   ,func ,@ts) (append* (map term-free-names ts)))))

(define (formula-free-names f)
  (match f
    (`(relate            ,r ,@ts)    (append* (map term-free-names ts)))
    (`(,(or 'exist 'all) ,params ,f) (define bound (list->set params))
                                     (filter-not (lambda (n) (set-member? bound n))
                                                 (formula-free-names f)))
    (`(,connective       ,@fs)       (append* (map formula-free-names fs)))))

(define (order-parameters params ordered-names)
  (let loop ((ordered-names ordered-names) (params (list->set params)))
    (match ordered-names
      ('()                       '())
      ((cons name ordered-names) (if (set-member? params name)
                                   (cons name (loop ordered-names (set-remove params name)))
                                   (loop ordered-names params))))))

(define-syntax-rule (rename-locally body ...) (parameterize ((name=>renamed (hash))
                                                             (renamed=>name (hash)))
                                                body ...))

(define name=>renamed (make-parameter #f))
(define renamed=>name (make-parameter #f))

(define (rename name)
  (define n=>n (name=>renamed))
  (or (hash-ref n=>n name #f)
      (let ((count (hash-count n=>n)))
        (name=>renamed (hash-set n=>n name count))
        (renamed=>name (hash-set (renamed=>name) count name))
        count)))

(define (unrename name) (hash-ref (renamed=>name) name))

(define (term-rename-variables term)
  (match term
    (`(quote ,_)       term)
    (`(var ,name)      `(var ,(rename name)))
    (`(app ,func ,@ts) `(app ,func . ,(map term-rename-variables ts)))))

(define (term-unrename-variables term)
  (match term
    (`(quote ,_)       term)
    (`(var ,name)      `(var ,(unrename name)))
    (`(app ,func ,@ts) `(app ,func . ,(map term-unrename-variables ts)))))

(define (formula-rename-variables formula)
  (match formula
    (`(relate                             ,r ,@ts)    `(relate      ,r . ,(map term-rename-variables ts)))
    (`(,(and (or 'exist 'all) quantifier) ,params ,f) (define free-names (formula-free-names f))
                                                      (for-each rename free-names)  ; allocate free names before bound names for readability
                                                      `(,quantifier ,(map rename (order-parameters params free-names))
                                                                    ,(formula-rename-variables f)))
    (`(,connective                        ,@fs)       `(,connective . ,(map formula-rename-variables fs)))))

(define (formula-unrename-variables formula)
  (match formula
    (`(relate                             ,r ,@ts)    `(relate      ,r . ,(map term-unrename-variables ts)))
    (`(,(and (or 'exist 'all) quantifier) ,params ,f) `(,quantifier ,(map unrename params)
                                                                    ,(formula-unrename-variables f)))
    (`(,connective                        ,@fs)       `(,connective . ,(map formula-unrename-variables fs)))))


;; TODO: before continuing, design data model with dataflow language for planning

;; Implementation phases:
;; - basic datalog
;;   - only these features
;;     - and (expressing joins)
;;     - or  (expressing unions)
;;     - ==, limited such that relations over == describe finite tables
;;       - (== constant constant) can be simplified out
;;         - could be a table with zero columns and either one or zero rows
;;       - (== var constant) is a table with a single row and column
;;       - no (== var1 var2) for now
;;     - recursive relations defined by least fixed point
;;   - no partial eval yet
;;   - none of these yet
;;     - not =/= < <= cons vector
;; - TODO: describe subsequent phases in order of increasing complexity

;; Compilation steps:
;; - parse
;; - simplify without serious inlining
;; - introduce shared/factored definitions for remaining code
;;   - one goal of factoring is to provide worker/wrapper separation for non-explosive inlining
;; - simplify with more serious inlining
;; - infer modes and [pre-]plan
;; - generate mode/plan-specialized code

;; Predictable simplification and inlining / partial eval
;; - ideas from macro writer's bill of rights:
;;   - the original list:
;;     - introduce let & lambda
;;     - ignore special cases involving constants
;;     - ignore degenerate cases resulting in dead/useless code
;; - local simplification
;;   - a single-branch disjunction extends the context directly
;;   - a multi-branch disjunction extends the context with the lattice-join of its branches
;;   - disjunction branches that always fail in the given context should be pruned
;;     - perform only one (left-to-right? or mode-specific?) pass to guarantee O(N) runtime
;;       - well, it may make sense to perform two passes, where the first simplification makes it
;;         easier to order constraints for an effective second pass
;;       - do not compute the potentially non-linear pruning fixed point when the context is extended
;;       - can have a separate optimization mode to compute the fixed point of such pruning
;;   - limited inter-disjunction simplification
;;     - fuse disjunctions when their branches can be joined at most one-to-one (i.e., no explosion)
;;     - disjunctions should *NOT* be cross-joined/DNFed in general
;;       - will quickly explode code size
;;       - can have a separate optimization mode to attempt general cross-disjunction simplification
;; - table-ification of table-like disjunctions
;;   - table-like means a disjunction of uniform (same variable structure) conjunctions of equality constraints
;; - inlining
;;   - inline calls to non-recursive relations that are small enough
;;   - inline calls to other relations based on analysis of how they are called
;;     - inline calls with known arguments that can lead to significant simplification
;;     - bottom-up analysis of how relation parameters are used
;;       - knowing the shape of a parameter may eliminate many disjunction branches
;;       - the relation may be structurally recursive on a subset of parameters
;;       - a parameter may not be scrutinized at all, i.e., it may be invariant

;; Factored relation definitions:
;; - a simple-relate is a call to a user-defined relation where all arguments are distinct variables
;; - a simple-cx is any of these:
;;   - a simple-relate
;;   - any call to a primitive constraint such as ==, =/=, <, <=
;; - post-factoring relation definition types:
;;   - single call to a relation with specialized arguments
;;     - some arguments are either partially known (not variables), or use the same variable more than once
;;   - conjunction of simple-cxs
;;   - disjunction of simple-cxs
;;   - negation of a simple-relate (not a simple-cx)
;;   - quantification of a simple-relate (not a simple-cx)

;; Notes and ideas on satisfiability:
;; - a conjunction including at most one disjunction is satisfiable if one disjunct is satisfiable
;; - for a conjunction including two or more disjunctions, satisfiability is only guaranteed via flattening into a single disjunction, where one disjunct is satisfiable
;;   - possible to flatten by joining:
;;     - disjunct-wise (always)
;;     - or variable-wise (only for variables that are constrained in every disjunct in all disjunctions currently being combined)
;;       - degenerate case: every disjunct applies the same constraint (however, possible to factor out this constraint due to being shared)
;;   - this search for a single satisfiable flattened disjunct may be arbitrarily expensive (may be searching for a needle in a haystack)
;;   - approaches that may be more tractable:
;;     - two-watched disjunctions:
;;       - each disjunction maintains two disjuncts that are locally satisfiable, along with a list of remaining disjuncts that haven't been tested yet
;;       - if either of these watched disjuncts becomes unsatisfiable, find another from the untested list, pruning any that fail
;;       - if only one satisfiable disjunct remains, it is promoted as an absolute part of the outer conjunction, used when filtering the other disjunctions
;;     - learning absolute/shared constraints across all disjuncts in a disjunction:
;;       - take the lattice join (analogous to an intersection across constraints) to find constraints that all disjuncts agree on
;;       - e.g. `(disj (== x (cons A B)) (== x (cons C B))) ==> (conj (fresh (D) (== x (cons D B))) (disj (== x (cons A B)) (== x (cons C B))))`
;;         - simplified: `(fresh (D) (conj (== x (cons D B)) (disj (== D A) (== D C))))`
;;     - subsumption
;;       - for each disjunct, starting with the smallest (least constrained), discard any other disjuncts that strictly include its constraints
;;     - variable-wise factoring of an individual disjunction
;;       - similar to variable-wise flattening, but without joining, instead building a new disjunction that looks like a decision tree
;;       - for each variable (often starting with the most constrained) that is constrained by every disjunct, bisect on a constraint choice (recursively)
;; - may unfold all relation calls except when doing so may lead to looping/redundancy
;;   - disjunctions annotated with procedure history stack, to recognize recursive calls
;;     - different history information granularities for different termination measures
;;       - e.g.,
;;         - no calls at all (least permissive, but no inlining achieved)
;;         - or no recursive calls (least permissive while still performing some inlining)
;;         - or no non-structural recursive calls (most permissive w/ guaranteed termination without programmer assistance)
;;         - or no non-decreasing recursive calls according to programmer-defined measure (most permissive w/ guaranteed termination)
;;         - any call is fine (most permissive, but no termination guarantees)

;; How to handle the non-datalog subset of computation:

;(struct store (var=>shape id=>cx) #:prefab)
;; - each constraint in id=>cx is a CNF formula
;;   - may include calls to user-defined relations that have not yet been unfolded
;;   - its id is associated with the shape of all mentioned variables
;; - a shape is either a t:quote, a t:cons, a t:vector, or a vcx
;; - for implementation simplicity, avoid more complicated variable-specific indexing schemes for now
;; - when a new constraint is inserted, apply simplification rules for any interacting variables
;; - when a variable shape is updated, apply simplification rules for any interacting cxs
;; - search for variable assignments that satisfy all cxs in id=>cx
;;   - for constraints participating in satisfaction:
;;     - run datalog sub-queries if possible
;;     - otherwise, unfold any calls to relations

;(struct vcx (lb ub cx-ids) #:prefab)
;; each of lb and ub is an interval endpoint, and either may be open or closed
;; cx-ids is a set of ids for the cxs that this variable interacts with
