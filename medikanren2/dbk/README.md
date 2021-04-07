# dbKanren

This implementation of Kanren supports defining, and efficiently querying,
large-scale relations.

Typical use:

```
(require "PATH-TO-DBKANREN/dbk.rkt")
```


## TODO

* implement checkpointing to avoid losing work with a long materialization process

* `explain` for extracting the database subset needed to reproduce a query's results
  * with each result, run an instrumented query to record supporting facts
  * there may be redundancy (one result could be computed in multiple ways)

* only register `c:table` with the subset of variables that are indexable
* factor out table choosing

* redesign tables, indexing, variable ordering heuristics
  * to get array-based lookup, specify appropriate column constraints
  * also, define tables that use column-oriented layout
  * identify most-constrained-variable more appropriately
    * simple cardinality (minimum-remaining-values) heuristic is not effective
    * cardinality is innaccurate since we only track bounds and don't eagerly
      maintain arc consistency
    * instead of MRV heuristic, a better heuristic considers how variables
      constrain relations
      * ~~rank relation constraints by the ratio of rows `remaining/total`~~
        * ~~a constraint's remaining rows are those whose first column value
          falls within the constraint's corresponding variable's bounds~~
        * smallest ratio wins, choose that constraint's first column variable
      * it may also make sense to consider all the ratios a variable
        participates in, and rank variables by some function on these ratios
        * e.g., could take minimum (described above), could multiply, etc.
    * we can speculatively compute a fixed number of members of a variable's
      finite domain, working inward from its bounds, to improve accuracy
      * helps accuracy of both MRV and most-constrained-relation heuristic
    * maybe augment this heuristic with consideration of a variable's
      index-path effectiveness (i.e., starting at this variable makes it easy
      to efficiently use other indexes)
      * see index-based path-finding below
  * maybe support intra-table/inter-table selectivity hints
    * intra: how many values of attr A do you get when setting attrs B, C, etc.?
      * degree constraints
      * could also declare that a column's values appear frequently
        * if they appear frequently, constraints to that column should not
          constrain other columns much
    * inter: `(conj (P a b c) (Q c d e))`
      * a form of join dependency
      * what proportion of distinct values `c` from each of `P` and `Q` remain
        after taking their intersection? e.g., maybe `(3/4 in P . 1/2 in Q)`
  * analyze, simplify, and plan with query structure
    * index-based path-finding to find candidate variable orderings
      * stick with one variable choice order until invalidation is possible
        * only long-distance constraints can invalidate assumptions
          * this includes monotone dependencies
      * when appropriate index paths aren't available, or query can be
        decomposed, consider materializing/indexing results for subqueries
    * articulation vertices for fast recognition of decomposable queries
      * may store intermediate results per subquery for faster integration

* redesign states, strategies to support adaptive analysis and optimization
  * ~~hypothetical states for analyzing and transforming disjunction components~~
    * should also use this approach for transforming arbitrary formulas
    * ~~process a disjunct's constraints in the usual way, but retain simplified
      residual constraints~~
      * optionally expand user-defined relations
    * may detect more opportunities for subsumption by reordering
    * may drop constraints for eliminated disjunct-local variables
      * e.g., simple equality constraints that have fully propagated
    * extract shared constraints from disjunction components via lattice-join
      * e.g., to implement efficient union of table constraints
  * decorate unexpanded user-defined relation constraints with the chain of
    already-expanded parent/caller relations that led to this constraint
    * to identify nonterminating loops
    * to help measure progress
  * optional bottom-up evaluation strategy
    * safety analysis of relations referenced during query/materialization
  * randomized variants of interleaving or depth-first search

* use small tables for finite domains
* fixed point computation

* place-based concurrency/parallelism?
* thread-safe table retrieval
* tee/piping output logs to file
* background worker threads/places for materialization
* documentation and examples
  * small tsv data example for testing materialization
* support an interactive stepping/user-choice "strategy"
  * both for debugging and devising new strategies

* how do we express columns of suffix type?
  * it would have this representation type: `#(suffix count len)`
  * but it would use a different comparison operator

* relation compilation to remove interpretive overhead
  * mode analysis
  * represent low-level operation representation depending on mode
    * e.g., == can act like an assignment, or a type/equality test
      * likewise for other constraint evaluations
  * partial evaluation
    * e.g., unify on partially-known term structures can be unrolled
    * constraint evaluations can be pre-simplified and reordered
  * code generation

* eventually, make sure relation metadata contains information for analysis
  * e.g., degree constraints, fast column ordering, subsumption tag/rules
  * descriptions used for subsumption
    * #(,relation ,attributes-satisfied ,attributes-pending)
    * within a relation, table constraint A subsumes B if
      B's attributes-pending is a prefix of A's
      AND
      B does not have any attributes-satisfied that A does not have
  * schema: heading, constraints, other dependencies
    * heading: set of attributes and their types
    * degree constraints (generalized functional dependencies)
      * interpret degree constraints to find useful special cases
        * functional dependency
        * bijection (one-to-one mapping via opposing functional dependencies)
        * uniqueness (functional dependency to full set of of attributes)
    * maybe join and inclusion dependencies
  * body: finite set of tuples

* floating point numbers are not valid terms
  * reordering operations endangers soundness
  * detect float literals when doing so won't hurt performance


### Data processing

* types
  * string/vector prefixes, suffixes, and slices
    * represented as pairs or vectors of nats
      * ID, start, end are all fixed-size nats
    * prefix: `(ID . end)`
    * suffix: `(ID . start)`
    * slice:  `#(ID start end)`
  * ~~binary/bytevector serialization format for compact storage and fast loading~~
    * more efficient numbers: polymorphic, int, rat
      * consider exponent-based representations when they would be more compact
  * string (and suffix) full-text search via bytes rather than chars?
    * and radix sorting

* high-level relation specification for transforming stream
  * high-level string transformation types: string, number, json, s-expression
    * perform this with Racket computation via `:==`
  * flattening of tuple/array (pair, vector) fields, increasing record arity
    * supports compact columnarization of scalar-only fields
  * generate helper relations for large or variable-length data columns
    * positional ID generation and substitution
    * replace strings/structures with unique IDs
    * IDs given by logical or file position
      * original value obtained by dereferencing ID appropriately
    * leads to compact and uniform record representations
  * job/task scheduler that automatically manages dependencies, validates
    integrity, and parallelizes relational stream transformation/tabulation
    * no need to manually coordinate stream processing tasks
    * config option for fast-replay byte encoding of input streams that need
      multiple passes

* ingestion
  * parsing: nq (n-quads)
  * gathering statistics
    * reservoir sampling
    * optional column type inference
    * per-type statistics for polymorphic columns
    * count, min, max, sum, min-length, max-length
    * histogram up to distinct element threshold
    * range bucketing
  * transformations specified relationally
    * possibly involving embedded Racket computation

* relational source data transformation
  * ability to attach metadata to relations, describing:
    * high-level column types, such as string suffix
    * transient or persistent status
    * memory usage budget/preferences


### Database representation

* metadata and data storage
  * hierarchical directory structure (generalizing catalog/schema/table org)
    * directories/relations are (un)loadable at runtime
      * causes dynamic extension/retraction of dependent intensional relations
      * later, support arbitrary insertion, deletion, and update
        * describe via temporal rules
  * record definition/dependency information in relation metadata
  * support export and import (and mounting) of database fragments

* intensional relations (user-level)
  * search strategy: backward or forward chaining
    * could be inferred
  * forward-chaining supports stratified negation and aggregation
  * materialization (caching/tabling)
    * if materialized, may be populated on-demand or up-front
    * partial materialization supported, specified per-rule
  * optionally temporal
    * each time-step, elements may be inserted or removed
      * update is remove and insert
      * some insertions may happen at non-deterministic times (async)

* extensional relations (user-level)
  * metadata
    * constraints:
      * degree (subsumes uniqueness, functional dependency, cardinality)
        * in relation R, given (A B C), how many (D E F)s are there?
          * lower and upper bounds, i.e., 0 to 2, exactly 1, at least 5, etc.
        * maybe support more precise constraints given specific field values
      * monotone dependencies
        * e.g., C is nondecreasing when sorting by (A B C D)
        * implies sorted rows for (C A B D) appear in same order as (A B C D)
          * one index can support either ordering
        * generalized: (sorted-columns (A B . C) (B . D) B) means:
          * B is already nondecreasing
          * C becomes nondecreasing once both A and B are chosen
          * D becomes nondecreasing once B is chosen (regardless of A)
    * statistics (derived from table statistics)

* low-level tables with optional keys/indices (not user-level)
  * disk/memory residence and memory structure reconfigurable at runtime
  * other types of tables:
    * suffix arrays: given order of the array describes sorted text
  * high-level semantic types
    * variable-width: logical position (ID) determined by offset table
      * text:        `string`
        * also possible to map text to a smaller alphabet for faster search
    * fixed-width: logical position (ID) = file-position / width
      * text suffix: `(ID . start-pos)`
  * metadata
    * ~~integrity/consistency checking~~
      * ~~source files (csvs or otherwise) with their size/modification-time~~
        * ~~element type/transformations~~
        * maybe statistics about their content
    * files/types and table dependencies
      * text suffix:
        * suffix file
          * `(#(nat ,text-ID-size) . #(nat ,pos-size))`
        * text column table
    * statistics


### Relational language for rules and queries

* functional term sublanguage
  * ~~atoms and constructors can be appear freely~~
  * ~~other computation appears under `:==`~~
    * ~~should be referentially transparent~~
    * ~~e.g., subqueries used for aggregation, implicitly grouped by outer query~~
  * maybe indicate monotonicity for efficient incremental update
    * by default, `:==` will stratify based on dependencies

* relations
  * local relation definitions to share work (cached results) during aggregation
    * `(let-relations (((name param ...) formula ...) ...) formula ...)`
  * if `r` is a relation:
    * `(r arg+ ...)` relates `arg+ ...` by `r`
    * `(relations-ref r)` accesses a metaprogramming control structure
      * configure persistence, representation, uniqueness and type constraints
      * indicate evaluation preferences or hints, such as indexing
      * dynamically disable/enable
      * view source, statistics, configuration, preferences, other metadata
  * data retention modes
    * caching/persistence may be forced or performed lazily
    * from most to least retention:
      * persistent
      * cached results
      * cached constraint states
      * cached analysis/plan
      * recomputed from scratch

* misc conveniences
  * `apply` to supply a single argument/variable to an n-ary relation
  * underscore "don't care" logic variables
  * tuple-column/record-field keywords
    * optional keyword argument calling convention for relations
    * optional keyword projection of query results
  * relation extension?

* evaluation
  * query evaluation
    * precompute relevant persistent/cached safe relations via forward-chaining
    * safe/finite results loop (mostly backward-chaining)
      * prune search space top-down to shrink scale of bulk operations
        * expand recursive relation calls while safe
          * safety: at least one never-increasing parameter is decreasing
            * this safety measure still allows polynomial-time computations
            * may prefer restricting to linear-time computations
            * or a resource budget to limit absolute cost for any complexity
          * track call history to measure this
          * recursive calls of exactly the same size are considered failures
        * keep results of branching relation calls independent until join phase
      * ~~maintain constraint satisfiability
        * ~~constraint propagation loop: `state-enforce-local-consistency`~~
          * ~~cheap first-pass via domain consistency, then arc consistency~~
          * backjump and learn clauses when conflict is detected
        * ~~then global satisfiability via search~~
          * ~~choose candidate for the most-constrained variable~~
            * ~~maybe the variable participating in largest number of constraints~~
          * ~~interleave search candidate choice with cheap first-pass methods~~
      * perform the (possibly multi-way) join with lowest estimated cost
      * repeat until either all results computed or only unsafe calls remain
    * perform unsafe interleaving until finished or safe calls reappear
      * like typical miniKanren search
      * if safe calls reappear, re-enter safe/finite results loop
  * safety (and computation) categories for relations
    * simple
      * persistent (extensional or precomputed intensional)
        * compute via retrieval
      * nonrecursive
        * compute via naive expansion
      * "safe" in the datalog sense
        * compute fixed point via semi-naive eval after top-down rewriting
          * also be able to compute fixed point via DFS w/ repetition check
        * all relations used in the body are safe
        * all head parameter vars mentioned in positive position in body
          * i.e., all head parameters will be bound to ground values
        * no constructors containing vars in "dangerous" positions
          * head parameters of recursive relation
          * arguments to recursive relations in body
        * aggregation is stratified
      * "unsafe": top-down interleaving search
        * no safety guarantees at all
          * refutational incompleteness
          * unlimited answers
          * answers with unbound variables, possibly with constraints
    * more complex alternatives
      * "safe" for forward chaining only
        * compute fixed point via semi-naive eval
        * generalizes "safe" datalog slightly
          * constructors w/ vars allowed as args to recursive relations in body
      * "safe" for backward chaining only
        * compute via DFS
        * may or may not guarantee all variables are bound (groundness)
        * termination guaranteed by structural recursion argument metric

* mode analysis
  * modes as binding patterns with result cardinalities
    * parameter binding classes from least to most constraining
      * f: free        (no guarantees of any kind)
      * x: constrained (if it's a variable, it at least has some constraints)
      * c: constructed (not a variable, but not necessarily fully ground)
      * b: bound       (fully ground)
    * appendo: (b f f) -> (0 1) (b f b)
      * bound first param leads to 0 or 1 result with bound third param
  * mode-specific safety
    * appendo termination requires (c f f) or (f f c) at every recursive call
  * mode-specific cost
    * different parameter bindings determine effectiveness of indices

* lazy population of text/non-atomic fields
  * equality within the same shared-id column can be done by id
    * also possible with foreign keys
    * analogous to pointer address equality
  * equality within the same nonshared-id column must be done by value
    * equal if ids are the same, but may still be equal with different ids
  * equality between incomparable columns must be done by value
    * address spaces are different
  * how is this integrated with mk search and query evaluation?
    * can do this in general for functional dependencies
      * `(conj (relate-function x a) (relate-function x b))` implies `(== a b)`
  * would like similar "efficient join" behavior for text suffix indexes
    * given multiple text constraints, find an efficient way to filter
      * will involve intersecting a matching suffix list for each constraint
      * one suffix list may be tiny, another may be huge
        * rather than finding their intersection, it's likely more efficient
          to just throw the huge one away and run the filter directly on the
          strings for each member of the tiny suffix set
    * how is this integrated with mk search and query evaluation?
      * text search can be expressed with `string-appendo` constraints
        * `(conj (string-appendo t1 needle t2) (string-appendo t2 t3 hay))`
      * suffix index can be searched via
        * `(string-appendo needle t1 hay-s)`
      * multiple needles
        * `(conj (string-appendo n1 t1 hay-s) (string-appendo n2 t2 hay-s))`
      * maybe best done explicitly as aggregation via `:==`


## Naming conventions

```
_    = blank
x-y  = x <space> y     ; for multi-word phrases, e.g., launch-all-missiles
x/y  = x with y
x^y  = x superscript y
x.y  = x subscript y   ; emphasizing grouping by x
y:x  = x subscript y   ; emphasizing grouping by y, the subscript; e.g., a type
                       ; y with constructors x, or implementing a common
                       ; interface of operations x, or other situations where
                       ; if the context is clear, the y: could be dropped from
                       ; the name
x@y  = x at y          ; result of projection or access using address/key y
x->y = x to y          ; procedure mapping type x to y
x=>y = x to y          ; finite map (e.g., hash or vector) with key type x
x&y  = x and y         ; a pair, or sometimes a 2-element list or vector
x*   = 0 or more xs    ; (typically homogeneous) lists, sometimes vectors
x?   = x huh           ; x is either a boolean(-ish) value itself, or a
                       ; predicate (procedure returning a boolean(-ish) value)
x!   = x bang          ; x may cause important side effects, such as mutation
                       ; or throwing an error; I/O operations often don't use
                       ; this convention
x?!  = assert x        ; a predicate/guard that throws an error if false
```
