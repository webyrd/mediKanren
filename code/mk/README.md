# miniKanren-with-symbolic-constraints

A revision of https://github.com/webyrd/miniKanren-with-symbolic-constraints/ for better performance. Up to 10x faster for large queries involving heavy use of constraints.

Includes `==`, `=/=`, `symbolo`, and `numbero`. `absento` is included, but the argument is required to be an eqv-comparable ground atom.

Eigen was removed.


## Using miniKanren

Run `scheme`, then load miniKanren with a path relative to your current directory.  From this directory, run:

```
(load "mk.scm")
```


## Running Tests

Run `scheme --script test-all.scm` from this directory.

To interact with relational arithmetic, type inferencer, and interpreter, run `scheme` from this directory and load the appropriate files.

For example, to load the interpreter and generate three quines, run the following in `scheme`:

```
(load "evalo.scm")
(run 3 (q) (evalo q q))
```
