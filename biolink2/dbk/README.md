# dbKanren

This implementation of Kanren supports defining, and efficiently querying,
large-scale relations.


## TODO

* materialized-relation
  * eventually, tables (primary or index) will be independent helper relations
  * but for now, the main relation will directly join with appropriate helper
    tables to avoid performance issues with naive DFS strategy

* stop using cells: procs should return mk syntax or opaque constraints
  * opaque relations (their procs return (constrain (relate ,proc) ,args))
  * interpretations (possibly for multiple evaluation strategies) should be
    attached to the registry dictionary, not the cell
    * compilation will remove registry lookup overhead
  * eliminate (constrain (retrieve ,s) ,args) and instead interpret the opaque
    constraint according to the evaluation strategy
* revert to a purely functional mk interpretation with a complete search
  * safer interaction between concurrent evaluation/analysis of shared queries
  * redefine var as immutable syntax (should not include a mutable value)
    * for optimizations like mutable var cells, compile to a new representation
* domain constraints
  * a var's possible values are the intersection of one or more as bounded sets
    * disagreeing bounds are refined by incremental intersection
  * discrete values from a finite relation
  * continuous ranges of values from an infinite relation
    * types as ranges of values
  * disequality constraints punch holes in continuous ranges
  * descriptions for subsumption and/or simplification with other constraints

* define tables that use column-oriented layout

* how do we express columns of suffix type?
  * it would have this representation type: `#(suffix count len)
  * but it would use a different comparison operator


### Data processing

* types
  * string/vector prefixes, suffixes, and slices
    * represented as pairs or vectors of nats
      * ID, start, end are all fixed-size nats
    * prefix: `(ID . end)`
    * suffix: `(ID . start)`
    * slice:  `#(ID start end)`
  * binary/bytevector serialization format for compact storage and fast loading
    * more efficient numbers: polymorphic, neg, int, float
  * string (and suffix) full-text search via bytes rather than chars?
    * and radix sorting

* high-level relation specification for transforming stream
  * high-level string transformation types: string, number, json, s-expression
    * perform this with Racket computation via `use`
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
    * try automatic goal reordering based on cardinality/statistics
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
      * sorted-columns: C is nondecreasing when sorting by (A B C D)
        * implies sorted rows for (C A B D) appear in same order as (A B C D)
          * one index can support either ordering
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
    * integrity/consistency checking
      * source files (csvs or otherwise) with their size/modification-time
        * element type/transformations, maybe statistics about their content
    * files/types and table dependencies
      * text suffix:
        * suffix file
          * `(#(nat ,text-ID-size) . #(nat ,pos-size))`
        * text column table
    * statistics


### Relational language for rules and queries

* functional term sublanguage
  * atoms and constructors can be appear freely
  * other computation appears under `use`
    * should be referentially transparent
    * e.g., subqueries used for aggregation, implicitly grouped by outer query
  * maybe indicate monotonicity for efficient incremental update
    * by default, `use` will stratify based on dependencies

* relations
  * `(define-relation (name param ...) goal ...) => (define name (relation (param ...) goal ...))`
  * `(define-relation/data (name param ...) data-description)`
  * `(relation (param-name ...) goal ...)`
  * `(use (relation-or-var-name ...) term-computation ...)`
    * computed term that indicates its relation and logic variable dependencies
    * force vars to be grounded so that embedded Racket computation succeeds
    * force dependency on given relations to ensure stratification
  * local relation definitions to share work (cached results) during aggregation
    * `(let-relations (((name param ...) goal ...) ...) goal ...)`
  * usual mk operators for forming goals:
    * `fresh`, `conde`, and constraints such as `==`, `=/=`, `symbolo`, etc.
    * `(r arg+ ...)` where `r` is a relation and `arg+ ...` are terms
  * if `r` is a relation:
    * `(r arg+ ...)` relates `arg+ ...` by `r`
    * `(r)` accesses a metaprogramming control structure
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
  * query results as lazy stream to enable non-materializing aggregation
  * tuple-column/record-field keywords
    * optional keyword argument calling convention for relations
    * optional keyword projection of query results
  * relation extension?

* evaluation
  * relational computation is performed in the context of a logic environment
    * logic environments are introduced by query and relation
    * logic environments are extended by fresh
  * query and other term computation is performed in the context of Racket
    * Racket environments are embedded in logic environments by `use`
    * can only occur during forward/bottom-up computation (ground variables)
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
      * maintain constraint satisfiability
        * cheap first-pass via domain consistency, then arc consistency
        * then global satisfiability via search
          * choose candidate for the most-constrained variable
            * maybe the variable participating in largest number of constraints
          * interleave search candidate choice with cheap first-pass methods
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
      * maybe best done explicitly as aggregation via `use`
