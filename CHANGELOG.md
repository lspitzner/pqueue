# Revision history for pqueue

## 1.4.1.3  -- 2020-06-06

  * Maintenance release
  * Add missing documentation
  * Add nix-expressions for testing against different compilers/package sets

## 1.4.1.2  -- 2018-09-26

  * Maintenance release for ghc-8.6
  * Drop support for ghc<7.10

## 1.4.1.1  -- 2018-02-11

  * Remove/Replace buggy insertBehind implementation.

    The existing implementation did not always insert behind. As a fix,
    the function was removed from Data.PQueue.Max/Min and was rewritten
    with a O(n) complexity (!) for Data.PQueue.Prio.Max/Min.

  * Adapt for ghc-8.4, based on the ghc-8.4.1-alpha1 release
  * Drop support for ghc<7.4

## 1.3.2.3  -- 2017-08-01

  * Maintenance release for ghc-8.2

## 1.3.2.2  -- 2017-03-12

  * Add test-suite from darcs repository for pqueue-1.0.1.

## 1.3.2.1  -- 2017-03-11

  * Fix documentation errors
    - complexity on `toList`, `toListU`
    - PQueue.Prio.Max had "ascending" instead of "descending" in some places

## 1.3.2    -- 2016-09-28

  * Add function `insertBehind` as a slight variation of `insert` which differs
    in behaviour for elements the compare equal.

## 1.3.1.1  -- 2016-05-21

  * Ensure compatibility with ghc-8
  * Minor internal refactors

## 1.3.1    -- 2015-10-03

  * Add Monoid instance for MaxPQueue

## 1.3.0    -- 2015-06-23

  * Lennart Spitzner starts co-maintaining
  * new git repository at github.com:lspitzner/pqueue
  * Ensure compatibility with ghc-7.10
