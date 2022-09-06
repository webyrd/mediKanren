# dbKanren

This implementation of Kanren supports defining, and efficiently querying,
large-scale relations.

Typical use:

```
(require "PATH-TO-DBKANREN/dbk.rkt")
```


## TODO

- Datalog implementation
  - Optional safety checking (we should still be able to execute unsafe programs)
    - Total (simple) safety: no function terms or aggregation through recursion
      - Guarantees safety regardless of evaluation strategy
    - When total safety fails, could try to determine safety just for top-down or bottom-up evaluation
    - More advanced checking would support recursion involving function terms and aggregation
      - Size-based termination checking for top-down recursion using function term arguments
      - Checking invariants that guarantee convergence for recursive aggregation
  - Demand transformation
    - Propagate query constants/bindings to determine needed demand signatures for each relation
    - For each relation's demand signature, schedule its body order, informed by database statistics
    - Generate the demand-transformed rules
    - Adjust naive evaluation to support stratified negation/aggregation w/ demand-transformed program
  - Semi-naive evaluation
    - Transform a program to use delta-based rules
    - Fixed-point calculation with 3 variants per recursive relation (old, delta, new)
  - Lowering to relational algebra operations
    - e.g., projecting away irrelevant variables to reduce intermediate duplication

- Relation definitions and queries involving semantic extensions
  - A relation defined with `define-relation` will raise an error if it does not support a finite mode
    - For Racket-embedded definitions, the error cannot be issued immediately due to
      mutually-recursive relations that may not be fully defined.  Instead, the error will be issued
      at the earliest query or request for compilation.
    - Use `define-search-relation` to suppress the error
    - e.g.,
      - Each pure relation has one finite mode: all attributes can be safely unknown, aka `#f`
      - Terminating computational relations may have more complex modes
        - These are recognized automatically by mode inference
        - `(appendo xs ys xsys)` has two finite modes: `(#t #t #f)` and `(#f #f #t)`
        - `(membero x xs)` has one finite mode: `(#f #t)`
      - `(evalo expr env result)` has no finite modes, so it's considered a search-based relation
        - Because exprs like `((lambda (d) (d d)) (lambda (d) (d d)))` are possible
  - A query executed with `run` will raise an error if it cannot guarantee termination
    - It may also provide diagnostics about worst-case behavior and execution progress
    - Use `run-search` to suppress the error

- Database improvements
  - Maybe we don't need the augmented 0th column for relations and incomplete indexes
    - Only index the listed columns?  Consider missing columns to have been projected away.
  - Try switching from fxvectors to using bytes where possible, and compress fixnums otherwise
    - `id=>id` remapping, and `col<->row` transposition with varying byte widths: annoying?
  - Improve checkpointing storage
    - Consolidate block files to use fewer
      - Make use of `(file-stream-buffer-mode in 'none)` file ports
        - `read-bytes` is as efficient as `file->bytes` if you know the `file-size` up front
      - May need a protocol for scattered/interrupted/fragmented writes to block ids
      - Will need a compaction protocol
    - Should we move away from block names and just use block ids?
  - Vectorized relational operators
  - Consider radix burst sort for ingesting text more quickly
    - Also consider alternatives to multi-merge for text

- Data access interface for queries
  - What form should RA-level (table-like) entities take?
    - Include database-path and a database-local UID for recognizing identitical entities
      - Important for using functional dependencies and other optimizations
    - Relation type (for eliminating pointless joins/unions/differences)
    - `index-ordering => (cons key-columns position-columns)`
      - Once a query is planned, this info is used to build the necessary dicts

- Try a simple text search strategy using a triples-containment relation
  - Don't build this into the database, this can all be user-defined
  - Filter name text to retain only lower-case alphanumeric (plus an empty value for 37 total values)
  - Build the relation: `(text-contains text c0 c1 c2)` or `(text-contains text compact-c012)`
    - Latter approach compacts c0 c1 c2 to a single 2-byte value
      - c0 c1 c2 are treated as digits
      - `compact-c012 = c0*37^2 + c1*37 + c2`
      - which is `< 52059`, which is `< 2^16`
    - Indexing `"foobar"` would produce these tuples:
      ```
      (text-contains "foobar" "f" "o" "o")
      (text-contains "foobar" "o" "o" "b")
      (text-contains "foobar" "o" "b" "a")
      (text-contains "foobar" "b" "a" "r")
      (text-contains "foobar" "a" "r" "_")
      (text-contains "foobar" "r" "_" "_")
      ```
    - Searching for `"test"` would use this query to produce a tractable number of candidates:
      ```
      (run* candidate
        (text-contains candidate "t" "e" "s")
        (text-contains candidate "e" "s" "t"))
      ```
    - Candidates can then be filtered using arbitrary rules defined in Racket

* a smaller extended syntax
  * logical connectives (not, and, or)
  * apply for relations
  * anonymous vars

* `explain` for extracting the database subset needed to reproduce a query's results
  * with each result, run an instrumented query to record supporting facts
  * there may be redundancy (one result could be computed in multiple ways)

- analyze, simplify, and plan with query structure
  - GOO join heuristic
  - index-based path-finding to find candidate variable orderings

* redesign states, strategies to support adaptive analysis and optimization
  * decorate unexpanded user-defined relation constraints with the chain of
    already-expanded parent/caller relations that led to this constraint
    * to identify nonterminating loops
    * to help measure progress
  * optional bottom-up evaluation strategy
    * safety analysis of relations referenced during query/materialization
  * randomized variants of interleaving or depth-first search

* use small tables for finite domains
* fixed point computation
  * binding signatures with constraints
    * share work by generalizing signatures

- improve `bounds` algebra by adding slightly-less and slightly-greater values for open interval endpoints
  - think of these as +epsilon, -epsilon
  - define comparison operators, min, and max, over these values

* place-based concurrency/parallelism?
* thread-safe table retrieval
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

- types
  - text suffix
    - suffix: `(ID . start)`
  - bigint, rational

* ingestion
  * parsing: nq (n-quads)
  * gathering statistics
    * reservoir sampling
    * optional column type inference
    * per-type statistics for polymorphic columns
    * count, min, max, sum, min-length, max-length
    * histogram up to distinct element threshold
    * range bucketing


### Database representation

- intensional relation search strategy: backward or forward chaining
  - safety-type could be inferred, or forced by parameter choice
  - can use a begin-like macro to set the safety parameter:
    - `(dbk-datalog-begin definitions ...)`
    - `(dbk-total-begin   definitions ...)`
  - forward-chaining supports stratified negation and aggregation

- metadata for extensional relations (user-level)
  - constraints:
    - degree (subsumes uniqueness, functional dependency, cardinality)
      - in relation R, given (A B C), how many (D E F)s are there?
        - lower and upper bounds, i.e., 0 to 2, exactly 1, at least 5, etc.
      - maybe support more precise constraints given specific field values
    - monotone dependencies
      - e.g., C is nondecreasing when sorting by (A B C D)
      - implies sorted rows for (C A B D) appear in same order as (A B C D)
        - one index can support either ordering
      - generalized: (sorted-columns (A B . C) (B . D) B) means:
        - B is already nondecreasing
        - C becomes nondecreasing once both A and B are chosen
        - D becomes nondecreasing once B is chosen (regardless of A)
  - statistics (derived from table statistics)


### Relational language for rules and queries

- indicate or infer aggregation monotonicity for efficient incremental update

- misc conveniences
  - `apply` to supply a single argument/variable to an n-ary relation
  - optional keyword argument calling convention for relations
    - user can implement this as Racket procedures that accept plist-style keyword args

* relations
  * local relation definitions to share work (cached results) during aggregation
    * `(let-relations (((name param ...) formula ...) ...) formula ...)`
    * express subqueries by capturing other vars
  * if `r` is a relation:
    * `(r arg+ ...)` relates `arg+ ...` by `r`
    * `(relations-ref r)` accesses a metaprogramming control structure
      * configure persistence, representation, uniqueness and type constraints
      * indicate evaluation preferences or hints, such as indexing
      * dynamically disable/enable
      * view source, statistics, configuration, preferences, other metadata

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
      * ~~maintain constraint satisfiability~~
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
- [Automatic Reordering for Dataflow Safety of Datalog](https://doi.org/10.1145/3236950.3236954)
- [Slides for Automatic Reordering for Dataflow Safety of Datalog](https://dodisturb.me/doc/slide/ppdp18.pdf)

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
