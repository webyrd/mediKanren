#lang racket/base
(provide
  m:named m:link m:term m:relation m:assert
  program:new program-remove program-flatten
  f:const f:relate f:implies f:iff f:or f:and f:not f:exist f:all
  f:any<= f:== f:=/=
  t:query t:map/merge t:quote t:var t:prim t:app t:lambda t:if t:let t:letrec
  t:apply t:cons t:car t:cdr t:vector t:list->vector t:vector-ref t:vector-length
  t-free-vars f-free-vars t-free-vars* t-free-vars-first-order t-free-vars-first-order*
  t-substitute f-substitute t-substitute* t-substitute-first-order t-substitute-first-order*
  f-relations t-relations t-relations*)
(require "misc.rkt" racket/hash racket/match racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; input and output devices are specified and created using host language
;; - may describe data coming from arbitrary input sources:
;;   - filesystem, network, channels, events, etc.
;;   - all sources are assumed to be unstable across time, reflecting a dynamic system
;;   - include metadata that will be retained in process history
;; - modules embed input and output device specifications in appropriate temporal relation declarations
;;   - input relations may only appear on the RHS of rules
;;   - output relations may only appear on the LHS of indeterminate inference rules
;;     - i.e., `<<~` (asynchronous send)

;; modules
;; - optional precomputation with indexing choices and retrieval preferences
;; - definitions for relations
;;   - rules for immediate inference: `define-relation` and/or `extend-relation`
;;     - deliver new facts during current time step, until fixed point is reached
;;     - synonym for `extend-relation`, deliver during current timestep: <<=
;;   - rules for next-step inference
;;     - delete at next timestep: <<-
;;     - insert at next timestep: <<+
;;   - rules for indeterminate inference
;;     - deliver at arbitrary (future?) timetep: <<~
;;     - these will write to output devices for the host system to process
;; - assertions: queries used for property/consistency checking or other validation
;;   - can inform data representation choices and query optimization
;;   - can use to infer:
;;     - uniqueness/degrees/cardinalities
;;     - value information (types, value ranges, frequencies)
;;     - join dependencies
;; - module linking
;;   - by default, a module will export all definitions
;;   - apply visibility modifiers (such as except, only, rename, prefix) to change a module's exports
;;     - renamings apply to a process too, so that its database relations can be targeted by new rules
;;     - support dependency shaking
;;       - given a root set of terms/relations, throw away everything else
;;   - combine compatible modules to produce a new module
;;     - same module may be linked more than once, to produce different variations (mixin-style)
;;       - for instance, this may be used to switch io devices

;; process state (like a version control system commit):
;; - reference to the dbms that manages this process state's database
;; - database
;;   - schemas
;;   - current content of all persistent temporal relations
;;     - i.e., (indexed) tables keyed by name
;;   - pending work (incomplete indexes)
;; - io device temporal relation bindings and buffers
;; - current program, describing how the process evolves each time step
;;   - mapping of public names to internal names
;;   - mapping of internal names to devices
;;   - mapping of internal names to tables
;;   - hierarchically-named subprograms w/ enabled/disabled status
;;   - in order to run, program must not have unresolved dependencies
;;     - terms without definitions are unresolved
;; - residual environment (when used as a library)
;;   - module specs inheriting from this process will be parsed using this environment
;; - history of database transitions
;;   - program changes
;;   - ingestion metadata per time step
;; - how do we check source data consistency?
;;   - processes allow data to change, so inconsistency with original sources may be intentional
;;   - to check consistency as in the old approach, analyze the process history for data provenance
;;     - particularly data io device bindings and their dependencies across time steps
;;       - input device metadata should include real world time stamps, filesystem information, transformation code, possibly content hash
;; - main operations:
;;   - run a query over the current database
;;   - change current program
;;     - enable/disable subprograms
;;     - rename/hide public names
;;     - hierarchy rearrangement and linking
;;     - nontemporal relations declared to be precomputed will be precomputed before returning
;;     - will add a program diff to the history
;;   - step, with a step/fuel count (`#f` to run continously)
;;     - will return unused fuel if (temporary) quiessence is detected, otherwise `#f`
;;     - if any work was performed, this will log a new database uid in the process history
;;     - if input was produced by devices configured for snapshot, snapshots will be included in the history
;;       - for replay reproducibility
;;   - synchronizable event indicating more work can be performed
;;     - e.g., if not even temporary quiessence has been achieved yet
;;     - e.g., if temporary quiessence had been achieved, but new input has arrived
;;     - `#f` if permanent quiessence has been reached
;;       - only possible if no input devices are bound
;;       - permanence w.r.t. the current program
;;   - save/flush (to dbms filesystem, for later reloading)
;;     - transitions should be continuously checkpointed and saved, so this may not be necessary
;;       - if background saving ends up being asynchronous, this operation will wait until the flush catches up
;;     - io devices cannot be directly restored from disk
;;       - their metadata can still be saved, however
;;       - when restoring such a process, io device rebindings must be provided

;; process (like a version control system branch):
;; - process state
;; - db name for stable reference in later program runs
;; - performing operations will update the current process
;;   - produce new process state and reassign stable name to the new state
;; - may spawn new process sharing the current process state
;;   - can explore diverging transitions
;;   - can save earlier state to later revisit
;;   - explicit garbage collection and compaction can be used to retain only data that is directly-referenced by active processes
;;     - can optionally preserve external data ingestion snapshots to support reproducing intermediate databases
;; - may be packaged for export to another dbms
;;   - dbms garbage collection and process export are similar activities

;; dbms (database management system):
;; - collection of named process databases
;; - cached relation content keyed by history dependencies, to support sharing between similar processes
;;   - includes content of temporal relations as well as nontemporal relations declared to be cached/precomputed
;;   - cache may include term values that were expensive to compute
;;   - processes evolving from a common ancestor will share content unless rule extensions lead to logical divergence
;;     - extending a precomputed relation will require precomputing a new version to be consistent
;;       - not an error, but maybe provide a warning
;;       - progammer decides whether multiple precomputed versions of similar rules is worth the time/space trade off
;;         - programmer organizes modules according to this decision

;; modular stratification given a partial order on relation parameters
;; - would be convenient to define a universal <= that respected point-wise monotonicity (any<= does not)
;; - reachability example using equivalence classes reduces materialized space usage from O(n^2) to O(n):
;;     (define-relation/source (node n)
;;       ;; specify graph vertex data here
;;       )
;;     (define-relation/source (arc a b)
;;       ;; specify graph connection data here
;;       )
;;
;;     ;; original definition of reachable before optimization
;;     ;; materialization could take O(n^2) space
;;     (define-relation (reachable a b)
;;       (conde
;;         ((node a) (== a b))
;;         ((fresh (mid)
;;            (reachable a mid)
;;            (conde ((arc mid b))
;;                   ((arc b mid)))))))
;;
;;     ;; new definition of reachable after optimization
;;     (define-relation (reachable a b)
;;       (fresh (repr)
;;         (reachable-class repr a)
;;         (reachable-class repr b)))
;;
;;     ;; materialization will take O(n) space
;;     (define-relation (reachable-class representative x)
;;       (string<= representative x)  ;; not required, but does this improve performance?
;;       ;; This negated condition ensures we represent each reachability class only once, to save space.
;;       ;; self-recursion within negation is possible due to modular stratification by string<
;;       ;; i.e., (reachable-class r x) only depends on knowing (reachable-class p _) for all (string< p r), giving us a safe evaluation order
;;       (not (fresh (predecessor)
;;              (string< predecessor representative)
;;              (reachable-class predecessor representative)))
;;       (conde
;;         ((node representative) (== representative x))
;;         ((fresh (mid)
;;            (reachable-class representative mid)
;;            (conde ((arc mid x))
;;                   ((arc x mid)))))))

;; ACILG hierarchy for analysis and optimization
;; - associative, commutative, idempotent, has-least-element, has-greatest-element
;; - comprehensions: map followed by combining mapped results
;; - an operator having all the properties in a prefix of this list may support more optimization
;; - associative: parallelism, but may need coordination for ordering
;; - commutative: parallelism, no ordering coordination needed
;; - idempotent:  fixed point computation without needing stratification; may need an initial value
;; - has-least-element: natural choice of initial value already known
;; - has-greatest-element: some computation may be stopped early, before all data is seen, if threshold is reached

;; order-by for converting sets/dicts to sequences

;; compact formulas as state component?
;; - state is a strategy-agnostic compact formula, plus a strategy-specific component (including a work scheduler)
;; - multi-pass strategy compilation: describe this state representation in the AST for the next pass's language

;; In some untyped systems, term evaluation may produce side-conditions (formulas)
;; - e.g., (car x) may introduce the formula (pair? x)

;; complex terms should be replaced with fresh variables to avoid redundant computations

;; more specific variable domain constraints:
;; - constant
;; - non-constant data construction (cons, vector, ...)
;;   - shape may not be fully known (vector with unspecified length)
;; - finite domain
;;   - could be represented as a join of constants; probably better to use a small table
;;   - could also include a representation for semi-joined table constraints
;; - interval domain
;;   - possibly with gaps/subtractions (negative finite domains)
;; - join of domain constraints

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-variant formula?
  (f:const   value)  ; can be thought of as a relation taking no arguments
  (f:relate  relation args)
  (f:implies if then)
  (f:iff     f1 f2)
  (f:or      f1 f2)
  (f:and     f1 f2)
  (f:not     f)
  (f:exist   params body)
  (f:all     params body))

(define (f:any<= u v) (f:relate '(prim any<=) (list u v)))
(define (f:any<  u v) (f:relate '(prim any<)  (list u v)))
(define (f:==    u v) (f:relate '(prim ==)    (list u v)))
(define (f:=/=   u v) (f:relate '(prim =/=)   (list u v)))

;; lambda calculus extended with constants (quote), logical queries, map/merge comprehensions
(define-variant term?
  (t:query     name formula)
  (t:map/merge proc.map proc.merge default xs)
  (t:quote     value)
  (t:var       name)
  (t:prim      name)
  (t:app       proc args)
  (t:lambda    params body)
  (t:if        c t f)
  (t:let       bpairs body)
  (t:letrec    bpairs body))

(define (t:apply f . args)  (t:app (t:prim 'apply)         args))
(define (t:cons a d)        (t:app (t:prim 'cons)          (list a d)))
(define (t:car p)           (t:app (t:prim 'car)           (list p)))
(define (t:cdr p)           (t:app (t:prim 'cdr)           (list p)))
(define (t:vector . args)   (t:app (t:prim 'vector)        args))
(define (t:list->vector xs) (t:app (t:prim 'list->vector)  (list xs)))
(define (t:vector-ref v i)  (t:app (t:prim 'vector-ref)    (list v i)))
(define (t:vector-length v) (t:app (t:prim 'vector-length) (list v)))

;; TODO: use CPS yielding to efficiently support partial-answer variations
(define (t-free-vars t (first-order? #f))
  (let loop ((t t))
    (match t
      ((t:query  name f)      (set-subtract (f-free-vars f first-order?) (set name)))
      ((t:quote  _)           (set))
      ((t:var    name)        (set name))
      ((t:prim   _)           (set))
      ((t:app    func args)   (set-union (t-free-vars* args first-order?)
                                         (if first-order? (set) (loop func))))
      ((t:lambda params body) (set-subtract (loop body) (list->set params)))
      ((t:if     c t f)       (set-union (loop c) (loop t) (loop f)))
      ((t:let    bpairs body) (set-union (t-free-vars* (map cdr bpairs) first-order?)
                                         (set-subtract (loop body) (list->set (map car bpairs)))))
      ((t:letrec bpairs body) (set-subtract (set-union (t-free-vars* (map cdr bpairs) first-order?)
                                                       (loop body))
                                            (list->set (map car bpairs)))))))

(define (t-free-vars-first-order t) (t-free-vars t #t))

(define (f-free-vars f (first-order? #f))
  (let loop ((f f))
    (match f
      ((f:const   _)             (set))
      ((f:or      f1 f2)         (set-union (loop f1) (loop f2)))
      ((f:and     f1 f2)         (set-union (loop f1) (loop f2)))
      ((f:implies if then)       (set-union (loop if) (loop then)))
      ((f:relate  relation args) (t-free-vars* args first-order?))
      ((f:exist   params body)   (set-subtract (loop body) (list->set params)))
      ((f:all     params body)   (set-subtract (loop body) (list->set params))))))

(define (t-free-vars* ts (first-order? #f))
  (foldl (lambda (t vs) (set-union vs (t-free-vars t first-order?)))
         (set) ts))
(define (t-free-vars-first-order* ts) (t-free-vars* ts #t))

(define (f-relations f)
  (match f
    ((f:const   _)             (set))
    ((f:or      f1 f2)         (set-union (f-relations f1) (f-relations f2)))
    ((f:and     f1 f2)         (set-union (f-relations f1) (f-relations f2)))
    ((f:implies if then)       (set-union (f-relations if) (f-relations then)))
    ((f:relate  relation args) (set-add (t-relations* args) relation))
    ((f:exist   params body)   (f-relations body))
    ((f:all     params body)   (f-relations body))))

(define (t-relations t)
  (match t
    ((t:query  _ f)         (f-relations f))
    ((t:quote  _)           (set))
    ((t:var    _)           (set))
    ((t:prim   _)           (set))
    ((t:app    func args)   (set-union (t-relations func) (t-relations* args)))
    ((t:lambda params body) (t-relations body))
    ((t:if     c t f)       (set-union (t-relations c) (t-relations t) (t-relations f)))
    ((t:let    bpairs body) (set-union (t-relations* (map cdr bpairs)) (t-relations body)))
    ((t:letrec bpairs body) (set-union (t-relations* (map cdr bpairs)) (t-relations body)))))

(define (t-relations* ts)
  (foldl (lambda (t rs) (set-union rs (t-relations t)))
         (set) ts))

(define (t-substitute t name=>name (first-order? #f))
  (let loop ((t t))
    (match t
      ((t:query  name f)      (t:query name (f-substitute f (hash-remove name=>name name) first-order?)))
      ((t:quote  _)           t)
      ((t:var    name)        (t:var (hash-ref name=>name name name)))
      ((t:prim   _)           t)
      ((t:app    func args)   (t:app func (t-substitute* args name=>name first-order?)))
      ((t:lambda params body) (t:lambda params (t-substitute body
                                                             (hash-remove* name=>name params)
                                                             first-order?)))
      ((t:if     c t f)       (t:if (loop c) (loop t) (loop f)))
      ((t:let    bpairs body) (define params (map car bpairs))
                              (t:let (map cons params (t-substitute* (map cdr bpairs)
                                                                     name=>name
                                                                     first-order?))
                                     (t-substitute body
                                                   (hash-remove* name=>name params)
                                                   first-order?)))
      ((t:letrec bpairs body) (define params (map car bpairs))
                              (define n=>n   (hash-remove* name=>name params))
                              (t:let (map cons params (t-substitute* (map cdr bpairs)
                                                                     n=>n
                                                                     first-order?))
                                     (t-substitute body
                                                   n=>n
                                                   first-order?))))))

(define (t-substitute* ts name=>name (first-order? #f))
  (map (lambda (t) (t-substitute t name=>name first-order?)) ts))

(define (f-substitute f name=>name (first-order? #f))
  (let loop ((f f))
    (match f
      ((f:const   _)             f)
      ((f:or      f1 f2)         (f:or      (loop f1)
                                            (loop f2)))
      ((f:and     f1 f2)         (f:and     (loop f1)
                                            (loop f2)))
      ((f:implies if then)       (f:implies (loop if)
                                            (loop then)))
      ((f:relate  relation args) (f:relate relation (t-substitute* args name=>name first-order?)))
      ((f:exist   params body)   (f:exist params (f-substitute body
                                                               (hash-remove* name=>name params)
                                                               first-order?)))
      ((f:all     params body)   (f:all   params (f-substitute body
                                                               (hash-remove* name=>name params)
                                                               first-order?))))))

(define (t-substitute-first-order  t  name=>name) (t-substitute  t  name=>name #t))
(define (t-substitute-first-order* ts name=>name) (t-substitute* ts name=>name #t))

;; A schema is a finite map of names to finite maps of properties to sets of
;; values, i.e.: (=> name (=> property (set value)))
(define schema.empty (hash))
(define (schema:new private=>property=>value)
  (make-immutable-hash
    (hash-map private=>property=>value
              (lambda (private p=>v)
                (cons private
                      (make-immutable-hash
                        (hash-map p=>v (lambda (p v) (cons p (set v))))))))))

(define (schema-union p=>p=>v.0 p=>p=>v.1)
  (hash-union p=>p=>v.0 p=>p=>v.1
              #:combine (lambda (p=>v.0 p=>v.1)
                          (hash-union p=>v.0 p=>v.1 #:combine set-union))))

(record module (terms relations assertions name=>submodule) #:prefab)
(define module.empty (module
                       (terms           schema.empty)
                       (relations       schema.empty)
                       (assertions      (set))
                       (name=>submodule (hash))))

(define (m:link     ms)        (foldl (lambda (m m.0)
                                             (match-define (module:struct ts.0 rs.0 as.0 n=>s.0) m.0)
                                             (match-define (module:struct ts   rs   as   n=>s)   m)
                                             (module
                                               (terms           (schema-union ts.0 ts))
                                               (relations       (schema-union rs.0 rs))
                                               (assertions      (set-union    as.0 as))
                                               (name=>submodule (hash-union n=>s.0 n=>s #:combine
                                                                            (lambda (s.0 s)
                                                                              (m:link (list s.0 s)))))))
                                           module.empty
                                           ms))
(define (m:named    name m)    (module:set module.empty (name=>submodule (hash       name m))))
(define (m:term     name p=>v) (module:set module.empty (terms           (schema:new (hash name p=>v)))))
(define (m:relation name p=>v) (module:set module.empty (relations       (schema:new (hash name p=>v)))))
(define (m:assert   formula)   (module:set module.empty (assertions      (set        formula))))

(define (module-remove m paths)
  (module:set m (name=>submodule
                  (foldl (lambda (path n=>sub)
                           (match path
                             ((cons name paths) (hash-update n=>sub name (lambda (m) (module-remove m paths))))
                             (name              (hash-remove n=>sub name))))
                         (module-name=>submodule m)
                         paths))))

(define (module-flatten m)
  (m:link (cons (module:set m (module-name=>submodule (hash)))
                (map module-flatten (hash-values (module-name=>submodule m))))))

(record program (module env) #:prefab)
(define (program:new m env) (program (module m) (env env)))

(define (program-remove  p paths) (program:set p (module (module-remove  (program-module p) paths))))
(define (program-flatten p)       (program:set p (module (module-flatten (program-module p)))))

;; TODO: simplify within some context (which may bind/constrain variables)?
;(define (t-simplify t)
;  (match t
;    ((t:var   name)   t)
;    ((t:quote value)  t)
;    ((t:app   f args) (let ((args (map t-simplify args)))
;                        (if (andmap t:quote? args)
;                          (apply f args)
;                          (t:app f args))))))
