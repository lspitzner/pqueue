# Revision history for pqueue

## 1.5.0

  * Make mapping and traversal functions force the full data structure spine.
    This should make performance more predictable, and removes the last
    remaining reasons to use the `seqSpine` functions. As these are no longer
    useful, deprecate them.
    ([#103](https://github.com/lspitzner/pqueue/pull/103))

  * Deprecate `insertBehind`. This function does not play nicely with merges,
    we lack tests to verify it works properly without merges, it imposes a
    substantial maintenance burden on the rest of the package, and it is quite
    slow. ([#35](https://github.com/lspitzner/pqueue/issues/35))

  * Add pattern synonyms to work with `MinQueue` and `MinPQueue`.
    ([#92](http://github.com/lspitzner/pqueue/pull/92))

  * Make the `Data` instances respect the queue invariants. Make the
    `Constr`s match the pattern synonyms. Make the `Data` instance for
    `MinPQueue` work "incrementally", like the one for `MinQueue`.
    ([#92](http://github.com/lspitzner/pqueue/pull/92))

  * Add strict maps and traversals.

## 1.4.3.0 -- 2022-10-30

  * Add instances for [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable).
    ([#85](https://github.com/lspitzner/pqueue/pull/85))
  * Add ghc-9.4 support. ([#86](https://github.com/lspitzner/pqueue/pull/86))

## 1.4.2.0 -- 2022-06-19

  * Overall performance has improved greatly, especially when there are many
    insertions and/or merges in a row. Insertion, deletion, and merge are now
    *worst case* logarithmic, while maintaining their previous amortized
    bounds. ([#26](https://github.com/lspitzner/pqueue/pull/26))

  * New `mapMWithKey` functions optimized for working in strict monads. These
    are used to implement the `mapM` and `sequence` methods of `Traversable`.
    ([#46](https://github.com/lspitzner/pqueue/pull/46))

  * Define `stimes` in the `Semigroup` instances.
    ([#57](https://github.com/lspitzner/pqueue/pull/57))

  * Add strict left unordered folds (`foldlU'`, `foldlWithKeyU'`)
    and monoidal unordered folds (`foldMapU`, `foldMapWithKeyU`).
    ([#59](https://github.com/lspitzner/pqueue/pull/59))

  * New functions for adjusting and updating the min/max of a key-value
    priority queue in an `Applicative` context.
    ([#66](https://github.com/lspitzner/pqueue/pull/66))

  * Fixed `Data.PQueue.Max.map` to work on `MaxQueue`s.
    ([#76](https://github.com/lspitzner/pqueue/pull/76))

## 1.4.1.4 -- 2021-12-04

  * Maintenance release for ghc-9.0 & ghc-9.2 support
  * Change nix-setup to use the seaaye tool

## 1.4.1.3 -- 2020-06-06

  * Maintenance release
  * Add missing documentation
  * Add nix-expressions for testing against different compilers/package sets

## 1.4.1.2 -- 2018-09-26

  * Maintenance release for ghc-8.6
  * Drop support for ghc<7.10

## 1.4.1.1 -- 2018-02-11

  * Remove/replace buggy `insertBehind` implementation.

    The existing implementation did not always insert behind. As a fix,
    the function was removed from Data.PQueue.Max/Min and was rewritten
    with a O(n) complexity (!) for Data.PQueue.Prio.Max/Min.

  * Adapt for ghc-8.4, based on the ghc-8.4.1-alpha1 release
  * Drop support for ghc<7.4

## 1.3.2.3 -- 2017-08-01

  * Maintenance release for ghc-8.2

## 1.3.2.2 -- 2017-03-12

  * Add test-suite from darcs repository for pqueue-1.0.1.

## 1.3.2.1 -- 2017-03-11

  * Fix documentation errors
    - complexity on `toList`, `toListU`
    - `PQueue.Prio.Max` had "ascending" instead of "descending" in some places

## 1.3.2   -- 2016-09-28

  * Add function `insertBehind` as a slight variation of `insert` which differs
    in behaviour for elements the compare equal.

## 1.3.1.1 -- 2016-05-21

  * Ensure compatibility with ghc-8
  * Minor internal refactors

## 1.3.1   -- 2015-10-03

  * Add `Monoid` instance for `MaxPQueue`

## 1.3.0   -- 2015-06-23

  * Lennart Spitzner starts co-maintaining
  * new git repository at github.com:lspitzner/pqueue
  * Ensure compatibility with ghc-7.10
