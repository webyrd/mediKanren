# dbKanren

This implementation of Kanren supports defining, and efficiently querying,
large-scale relations.

Typical use:

```
(require "PATH-TO-DBKANREN/dbk.rkt")
```


## TODO

* local relation definitions
  * expressing subqueries by capturing other vars

* a smaller extended syntax
  * logical connectives (not, and, or)
  * apply for relations
  * anonymous vars

* support void values? (have to stop overloading the meaning of void everywhere)

* GOO join heuristic

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

* alternative variable ordering heuristic
  * joined?, unique? (approximate w/ single-column-remaining?)
    * or maybe specificity (via minimum relation cardinality?), cardinality
    * not all non-joined variables will be reachable, but worry about this later

* use small tables for finite domains
* fixed point computation
  * binding signatures with constraints
    * share work by generalizing signatures

* support hashes, sets, and procedures
  * if not orderable, can't embed in tables, but that could be fine
    * procedures probably aren't orderable, but sets and hashes could be
    * set elements may not be logic variables?
      * no, we can allow it
    * hash keys may not be logic variables?
      * no, we can allow it
  * `set->list` and `hash->list` will sort elements for determinism
    * implement them in terms of a numeric indexing operation
      * map each `i` in `0 <= i < count` to the appropriate key in sorted order

* improve bounds algebra by adding slightly-less and slightly-greater values for open interval endpoints
  * define comparison operators, min, and max, over these values

* simpler codec definition
  * simplified extracting/inserting of fixed or variable-length bit vectors
  * variable-length bit vectors may be encoded in a couple ways
    * a prefix describing length
      * if the prefix is itself fixed-size, this implies an upper bound on length
    * a UTF-8 style encoding with no upper bound on length

* place-based concurrency/parallelism?
* thread-safe table retrieval
* tee/piping output logs to file
* background worker threads/places for materialization
* documentation and examples
  * small tsv data example for testing materialization
  * LiveJournal, Orkut graph benchmark examples for
    * https://github.com/frankmcsherry/blog/blob/master/posts/2019-09-06.md
    * reachability
    * connected components
    * single-source shortest path
    * http://yellowstone.cs.ucla.edu/papers/rasql.pdf
    * http://pages.cs.wisc.edu/~aws/papers/vldb19.pdf
* support an interactive stepping/user-choice "strategy"
  * both for debugging and devising new strategies
  * `run/interactive`
    * same as a `run^` with `current-config` `search-strategy` set to `interactive`
    * non-stream, first-order representation of strategy-independent states
      * also support embedding and combining sub-states (hypothetical along a disjunct)
        * states are just fancy representations of conjunctions, and should be
          combinable/disolvable

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


### Aggregation ideas

* finite relations may relate their tuples to an extra position parameter
  * `(apply-relation-position R pos tuple)`

* aggregation via negation
  * can this be done efficiently?
  * edb: `(edge x y)`
  * ```
    (define-relation (1-hops x targets)
      (=/= targets '())
      (:== targets (query y (edge x y))))

    or

    (define-relation (1-hops x targets)
      (=/= targets '())
      (query== targets y (edge x y)))

    vs.

    (define-relation (1-hops-rest x prev targets)
      (conde
        ((== targets '())
         (not (fresh (z)
                (any< prev z)
                (edge x z))))
        ((fresh (y targets.rest)
           (== targets (cons y targets.rest))
           (edge x y)
           (not (fresh (z)
                  (any< prev z)
                  (any< z y)
                  (edge x z)))
           (1-hops-rest x y targets.rest)))))
    (define-relation (1-hops x targets)
      (fresh (y targets.rest)
        (== targets (cons y targets.rest))
        (edge x y)
        (not (fresh (z)
               (any< z y)
               (edge x z)))
        (1-hops-rest x y targets.rest)))
    ```

    ```
    (define-relation (max+o threshold x xs)
      (conde ((== xs '()) (== x threshold))
             ((fresh (y ys)
                (== xs (cons y ys))
                (conde ((any<= y threshold) (max+o threshold x ys))
                       ((any<  threshold y) (max+o y         x ys)))))))
    (define-relation (maxo x xs)
      (fresh (y ys)
        (== xs (cons y ys))
        (max+o y x ys)))
    (define-relation (edge-largest x largest)
      (fresh (targets)
        (query== targets y (edge x y))
        (maxo largest targets)))

    vs.

    (define-relation (edge-largest x largest)
      (edge x largest)
      (not (fresh (y) (any< largest y) (edge x y))))
    ```


### Query planning and joins

We need some low-level language for transformation (not for humans or direct interpretation).  Is this the right one?
- is there a more operational datalog-like notation? e.g., datalog + ordering hints?
  - (and _ ...) and (or _ ...) can be used to specify specific join/union order and nesting
  - but these seem hard, so this idea is probably not enough:
    - what about specifying particular kinds of joins?
    - what about projection and filtering?
    - what about aggregation?
    - how about sub-computation sharing and scope?

Initial sketch of relational algebra language for lower-level optimization transformations:
```
position ::= <natural number>
count    ::= <natural number>
merge-op ::= + | * | min | max | set-union | (min-union count) | (max-union count)
op       ::= cons | vector | list->vector | car | cdr | vector-ref | vector-length | merge-op
expr     ::= (quote value) | (ref position position) | (app op expr ...)
cmp      ::= == | any<= | any< | =/=
cx       ::= (cmp expr expr) | (and cx ...) | (or cx ...)
relation ::= (quote   ((value ...) ...)) ; What operator does this correspond to? Does it assume sorting/deduping? Probably no assumptions, just enumerate raw tuples.
           | (table   rid)               ; This corresponds to a basic table scan, use a different operator for more efficient access.
           | (rvar    name)
           | (filter  (lambda 1 cx) relation)
           | (project (lambda 1 (expr ...)) relation)
           | (union   relation ...)
           | (subtract relation relation)
           | (cross   relation ...)      ; TODO: replace with nested [for-]loop notation.

;; How do we specify n-way join shared keys?
;; New notation for join-key specification?
;; Operators for sorting (with direction), deduplication.
;; Operators for skipping (dropping), and limiting (taking).

           | (agg     relation (lambda 1 (expr ...)) value (lambda 1 (expr ...)) (merge-op ...))  ; rel group init map merge (map exprs and merge ops must be same length)
           | (let     ((name relation) ...) relation)  ; Relations are materialized, nesting describes stratification.
           | (letrec  ((name relation) ...) relation)  ; Relations computed as fixed points, relations are materialized, nesting describes stratification.

;; Let-bound relations should be specific about what physical tables/indexes are formed?

direction        ::= < | >
direction-suffix ::= () | #f
offset           ::= <natural number>
limit            ::= <natural number> | #f
query            ::= (query offset limit (direction ... . direction-suffix) relation)  ; Do we need this? Should we embed sorting, skipping, and limiting as operators instead?
```

Example query thought experiments:

Maybe bounds feedback doesn't make sense, and should be part of the plan ordering, where bounds actually come from the bottom rather than the top?
e.g., consider variable-centric plans for:
```
(run* (a b d e)
  (exist (c)
    (and (P a b c)
         (Q c d e)
         (R c d e))))

- c then (c d e) then c: (P ((P Q) (P R)))  ; aka semijoins
  - (join P [2] [1 2]
          (join (join P [2] [] Q [0] [0 1 2]) [0 1 2] [1 2]  ; this is a semijoin
                (join P [2] [] R [0] [0 1 2]) [0 1 2] [])    ; this is also a semijoin
          [0] [1 2])

- (c d e) then c: (P (Q R))
  - (join P [2] [1 2]
          (join Q [0 1 2] [1 2] R [0 1 2] [])  ; no semijoining performed
          [0] [1 2])

But neither of these gives adaptive feedback ...
Upstream and downstream can't communicate bounds with this approach.
Ideally we would choose an ordering like:
- c d e
or
- d e c

Binary joins seem too inflexible here.
```

Multi-join GOO perspective:
```
- (run* (a c) (exist (b) (P a b) (Q b c)))
  - max cardinality: (* (cardinality P a) (cardinality Q c))
- (run* (a b c) (P a b) (Q b c))
  - max cardinality: (min (* (cardinality P a)
                             (cardinality Q b c))
                          (* (cardinality Q c)
                             (cardinality P a b))
                          (* (cardinality P a)
                             (cardinality Q c)
                             (min (cardinality P b)
                                  (cardinality Q b))))
```

Naive selectivity for join cardinality estimation:
```
- |S join.x T| ~= (/ (* |S||T|) (max (cardinality S x) (cardinality T x)))
- can generalize for multi-way joins:
  - if joining k relations
    - numerator is a product of the k relation sizes
    - denominator is a product of the (- k 1) largest join-column cardinalities
```

Non-materializing multi-way joins may be wasteful:
```
- (P a b) (Q b c) (R c d) (S d e)
- joining variables: b c d
  - for each choice of b, we learn a set of cs and repeat some of the work of joining (R c d) (S d e) on d
  - the repeated work has two parts:
    - finding values of c where a satisfying (d e) exists
    - bisection revisitation overhead in enumerating these
  - if the result cardinality of (R c d) (S d e) is small, materializing this sub-join may be better
- semijoins as degenerate joins
  - (P a b) (Q b c) where P is used to reduce the size of Q based on b
  - equivalent to (P _ b) (Q b c)
  - Q^  = (P  _ b) (Q   b c)
  - R^  = (Q^ _ c) (R   c d)
  - S^  = (R^ _ d) (S   d e)
  - R^^ = (R^ c d) (S^  d _)
  - Q^^ = (Q^ b c) (R^^ c _)
  - P^  = (P  a b) (Q^^ b _)
  - arc-constrained query: (P^ a b) (Q^^ b c) (R^^ c d) (S^ d e)
    - benefit: increased cardinality estimation accuracy
```

Multi-way joins are not wasteful when all relations share the same join attributes (assuming proper indexes):
```
- (P x y a) (Q x b y) (R c y x)
- can intersect on either (x y) or (y x), and simply enumerate crosses of the non-join attributes
```

Multi-way joins may improve over binary joins whose expected result cardinality is greater than the max cardinality of either input.


### Database construction and management

A string dictionary (domain) on disk could involve 3 data files (with metadata in a separate file):
- metadata:
  - names of constituent files
  - number of strings in this dictionary
- id=>str
  - str.data: data file containing N variable-length strings
    - hello|world|more
    - N items
    - not sorted
    - no need to store string lengths (this is covered by the position file)
  - str.data.pos: position file for referencing data file (N + 1 positions)
    - 0|5|10|14
    - N+1 items
- str=>id
  - id.data: data file containing N ids, sorted alphabetically by the strings they refer to
  - N items
  - sorted indirectly by id => str.data.pos =interval=> str.data
    - lookup via binary search through this same indirection
- instead of this logical id format, we could save an indirection by using file positions as ids

- domains for variable-length-or-large-data
  - lazy merging of domains
  - write-ahead blocks maintain local domains until merged

- fixed-width data does not need a domain
  - null (but why?), bool, fixnat, fixint, fixrat, pair/fixed, tuple, fixarray (behaves like a tuple)

- atomic batch updates
  - single metadata file for entire database, providing a consistent view of the current state
    - batch updates will commit to this metadata file (at the next checkpoint)
  - not quite transactions, but good enough for now
  - inserted/deleted tuples based on a consistent database snapshot
  - insertion/deletion of single-relation blocks and domain+relation(s) write-ahead blocks
    - file path, file size, (file checksum?), block layout
    - garbage collection / export-of-current-db-state-snapshot

- initial domain insertion
  - should returned ids be the logical or the file positions?
    - if we load into RAM, file positions will still work on bytevectors
    - logical ids can save ~20% of single column memory if there are fewer than 4 billion strings
      - relatively small savings given small size of data?
      - may still be able to use other forms of compression on the non-logical ids
      - but may be worth it if queries tend to not cross databases, and new string data is not frequently inserted
  - domain sorting and possibly re-mapping ids
    - can either index ids based on string order
    - or maintain id assignment in string order directly
      - provides monotonic ids, which are more efficient when crossing dbs or emitting sorted string output
      - requires re-mapping the insertion ids to resulting monotonic ids in the original tuple data

- merge-unioning domain blocks
  - each block uses a different id basis, requiring id translation during merge
  - first index or monotonize ids block-locally
  - perform bisect/2-or-k-way merge-union, producing new domain column file
    - insert new logical or file position into remapping hashes for each original block
  - map the remapping tuple columns, producing new column files for each
    - if monotonic ids are used, indexes should not need to be re-sorted

- ideas for domain caching during block production, to minimize redundancy during sort and remap
  - maintain two datum=>id hash tables for these two pools: multiple-refs and single-ref-so-far
    - maybe parameterize a more general version: less-than-N-refs-so-far
      - in this case, treat as LRU, where seeing a new ref pushes a datum back onto the queue to
        preserve it for a little longer
      - maybe also give special treatment to small datums, which are less expensive to preserve?
  - treat single-ref-so-far as a fixed-size queue, where each datum&id pair is thrown away if
    a second ref isn't found before it rolls off the end
  - if a second ref is found for a datum in single-ref-so-far, it is promoted to multiple-refs, and
    stored for the rest of the production

- write-ahead block directory files:
  - one data file per domain (e.g., variable-length bytes for text, bigint, bigrat, or fixed-width pair/any)
    - pair/any is a 4-column table, where the car and cdr are annotated with type tags
    - more generally, a logical column type of "any" is itself 2 physical columns, one for the type tag
  - one position file per domain
    - potentially-segregated starting positions + one final ending position
  - one tuple file per update direction (insert or delete) per relation participating in update
    - if only inserting or deleting, that means one file, and if both, then two files for that relation
    - potentially-segregated fixed-width tuples that reference data positions
    - any segregations are due to mistakes in column-byte-width-guessing
  - metadata describing the contents of each of the other files
    - particularly the byte-widths of the position and tuple files, for each segregation, along with the
      segregation division locations

- once the initial write-ahead block is complete, it is committed to the database metadata, to avoid losing progress
  - each update is a transaction that either completely succeeds or fails

- sort domains
  - for each domain, this means producing a new file of sorted logical ids, which are themselves data file positions
    - the sorted order of these ids is implicitly an id remapping table
    - for each domain sorted, apply a checkpoint after that sorting process is finished
      - if the domain is large enough to require external sorting, apply finer-grained checkpoints as well
  - for each domain, merge into the database's main domain
    - merging domains produces 3 files:
      - new domain data file
      - old position remapping file
      - new (from the write-ahead block) position remapping file
        - for full write-ahead remapping, compose this with the remapping table produced by write-ahead domain sorting
    - checkpoint after each merge
- for each relation-delete and relation-insert file, remap its ids
  - checkpoint each of these
- remap each main db relation table not participating in update
  - checkpoint each of these remappings
- remap-merge each main db relation table participating in update
  - each of these is a 3-way merge between these sets: original, delete, insert
    - the delete and insert sets have already been remapped
    - tuples from the original set are remapped just-in-time
      - just-in-time will make bisection harder, so maybe not a good idea? remap ahead of time instead?
        - nah, this is probably fine
  - checkpoint each of these remapping-merges
- rebuild indexes for updated relations
  - checkpoint each of these

- to avoid manual code-commenting to deal with interrupted batch jobs, can also support a brute force insertion process
  - this way a small update-history relation can be used to describe and check the progress of a job
  - still single db consolidation, just brute-force read from the update-history relation

- for simplicity, let's start with relation-creation-time-single-insertion with errors and manual code-commenting for checkpoints
  - and single db consolidation, because the domains are shared


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


## References

- [miniKanren: an embedded DSL for logic programming](http://minikanren.org)
- [Bloom Programming Language](http://bloom-lang.net/)
- [Dedalus: Datalog in Time and Space](http://www2.eecs.berkeley.edu/Pubs/TechRpts/2009/EECS-2009-173.pdf)
- [Datafun Programming Language](http://www.rntz.net/datafun/)
- [Flix Programming Language](https://flix.dev/)

### Datalog and Bottom-up Evaluation Techniques

- [What You Always Wanted to Know About Datalog (And Never Dared to Ask)](https://personal.utdallas.edu/~gupta/courses/acl/papers/datalog-paper.pdf)
- [Integrating Datalog and Constraint Solving](https://biblio.ugent.be/publication/5646054/file/5646069.pdf)
- [Compilation Of Bottom-Up Evaluation For a Pure Logic Programming Language](https://researchcommons.waikato.ac.nz/bitstream/handle/10289/12882/thesis.pdf)
- [Bottom-Up Evaluation](http://users.informatik.uni-halle.de/~brass/lp07/c6_botup.pdf)
- [Magic Sets](http://users.informatik.uni-halle.de/~brass/lp07/c7_magic.pdf)

### Recursive Relations

- [A Cost Estimation Technique for Recursive Relational Algebra](https://hal.inria.fr/hal-03004218/document)
- [On the Optimization of Recursive Relational Queries: Application to Graph Queries](http://pierre.geneves.net/papers/mura-sigmod20.pdf)
- [Cost-Based Optimization for Magic: Algebra and Implementation](https://dsf.berkeley.edu/papers/sigmod96-magic.pdf)

### Query Plans

- [How Good Are Query Optimizers, Really?](https://www.vldb.org/pvldb/vol9/p204-leis.pdf)
- [Cardinality Estimation Done Right: Index-Based Join Sampling](http://cidrdb.org/cidr2017/papers/p9-leis-cidr17.pdf)
- [A New Heuristic for Optimizing Large Queries](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.31.737)
- [Looking Ahead Makes Query Plans Robust](http://www.vldb.org/pvldb/vol10/p889-zhu.pdf)
- [Worst-case Optimal Join Algorithms](http://pages.cs.wisc.edu/~chrisre/papers/paper49.Ngo.pdf)

### Low-level Efficiency

- [Sort vs. Hash joins](http://www.vldb.org/pvldb/vol7/p85-balkesen.pdf)
- [Efficiently Compiling Efficient Query Plans for Modern Hardware](http://www.vldb.org/pvldb/vol4/p539-neumann.pdf)
- [Push vs. Pull-Based Loop Fusion in Query Engines](https://arxiv.org/pdf/1610.09166.pdf)
- [How to Architect a Query Compiler, Revisited](https://www.cs.purdue.edu/homes/rompf/papers/tahboub-sigmod18.pdf)
