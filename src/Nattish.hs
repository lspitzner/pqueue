{-# LANGUAGE CPP #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 904
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
#endif

-- | A facility for faking GADTs that work sufficiently similarly
-- to unary natural numbers.
module Nattish
  ( Nattish (Zeroy, Succy)
  )
  where
import Foreign.Ptr (WordPtr)
import Unsafe.Coerce (unsafeCoerce)
#if __GLASGOW_HASKELL__ >= 800
import Data.Kind (Type)
#endif

-- | Conceptually,
--
-- @
-- data Nattish :: forall k. k -> (k -> k) -> k -> Type where
--   Zeroy :: Nattish zero succ zero
--   Succy :: !(Nattish zero succ n) -> Nattish zero succ (succ n)
-- @
--
-- This abstracts over the zero and successor constructors, so it can be used
-- in any sufficiently Nat-like context. In our case, we can use it for the @Zero@
-- and @Succ@ constructors of both @MinQueue@ and @MinPQueue@. With recent
-- versions of GHC, @Nattish@ is actually represented as a machine integer, so
-- it is very fast to work with.

#if __GLASGOW_HASKELL__ < 904
data Nattish :: k -> (k -> k) -> k -> * where
  Zeroy :: Nattish zero succ zero
  Succy :: !(Nattish zero succ n) -> Nattish zero succ (succ n)

toWordPtr :: Nattish zero succ n -> WordPtr
toWordPtr = go 0
  where
    go :: WordPtr -> Nattish zero succ n -> WordPtr
    go !acc Zeroy = acc
    go !acc (Succy n) = go (acc + 1) n

instance Show (Nattish zero succ n) where
  showsPrec p n = showParen (p > 10) $
    showString "Nattish " . showsPrec 11 (toWordPtr n)
#else

type Nattish :: forall k. k -> (k -> k) -> k -> Type
newtype Nattish zero succ n = Nattish WordPtr -- See [Note: WordPtr]
  deriving (Show)
type role Nattish nominal nominal nominal

data Res zero succ n where
  ResZero :: Res zero succ zero
  ResSucc :: !(Nattish zero succ n) -> Res zero succ (succ n)

check :: Nattish zero succ n -> Res zero succ n
check (Nattish 0) = unsafeCoerce ResZero
check (Nattish n) = unsafeCoerce $ ResSucc (Nattish (n - 1))

pattern Zeroy :: forall {k} zero succ (n :: k). () => n ~ zero => Nattish zero succ n
pattern Zeroy <- (check -> ResZero)
  where
    Zeroy = Nattish 0
{-# INLINE Zeroy #-}

pattern Succy :: forall {k} zero succ (n :: k). () => forall (n' :: k). n ~ succ n' => Nattish zero succ n' -> Nattish zero succ n
pattern Succy n <- (check -> ResSucc n)
  where
    Succy (Nattish n) = Nattish (n + 1)
{-# INLINE Succy #-}

{-# COMPLETE Zeroy, Succy #-}

-- [Note: WordPtr]
--
-- Why WordPtr and not Word? We want to be extremely certain that it does not
-- overflow, because that would lead to a type safety problem (accessing a Succ
-- object as a value). At first, I thought we couldn't possibly have to worry
-- about that—after all, the Nattish never exceeds the *logarithm* of the queue
-- size. But then I remembered that it's possible to make quite absurdly large
-- queues by repeated concatenation.  Imagine if a Word is 32 bits, but a
-- pointer is 64 bits. Then something like
--
--   stimes (2^(2^33)) (singleton () ())
--
-- might actually be possible to construct in a relatively reasonable amount of
-- time and an achievable (if somewhat ridiculous) amount of memory. If this
-- were then traversed with traverseWithKeyU or foldMapWithKeyU from right to
-- left (using a lazy left fold, for example), we could overflow. Disaster!
-- Fortunately, we have WordPtr, which is equivalent to uintptr_t, and
-- therefore big enough to hold the logarithm of the size of any queue
-- represented in memory, even with maximal sharing.
--
-- Now, according to a comment on
-- https://hackage.haskell.org/package/base-4.18.0.0/docs/src/Foreign.Ptr.html#WordPtr
-- GHC actually guarantees that a Word is the same size as a pointer, so we
-- actually *could* just use Word, but I think WordPtr makes it *utterly clear*
-- that we're safe here.

#endif
