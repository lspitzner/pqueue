# Revision history for pqueue

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
