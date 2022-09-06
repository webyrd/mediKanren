#lang racket/base

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
