{-# language
        DeriveFunctor
      , DerivingStrategies
      , EmptyCase
      , LambdaCase
      , BangPatterns
  #-}

module VoidT
  ( VoidT(..)
  ) where

import Data.Void

newtype VoidT m a = VoidT { runVoidT :: Void -> m a }
  deriving stock (Functor)

constVoid :: a -> Void
constVoid = \case {}

void :: Void
void = constVoid voidThunk

voidThunk :: a
voidThunk = error "VoidT.voidThunk: should not be evaluated!"
{-# noinline voidThunk #-}

destroy :: Functor f => f a -> VoidT f a
destroy f = VoidT (const f)

instance Foldable m => Foldable (VoidT m) where
  foldMap f (VoidT g) = foldMap f (g void)

instance Traversable m => Traversable (VoidT m) where
  traverse f (VoidT g) = fmap destroy (traverse f (g void))


