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

toWord :: Nattish zero succ n -> Word
toWord = go 0
  where
    go :: Word -> Nattish zero succ n -> Word
    go !acc Zeroy = acc
    go !acc (Succy n) = go (acc + 1) n

instance Show (Nattish zero succ n) where
  showsPrec p n = showParen (p > 10) $
    showString "Nattish " . showsPrec 11 (toWord n)
#else

type Nattish :: forall k. k -> (k -> k) -> k -> Type
newtype Nattish zero succ n = Nattish Word
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

#endif
