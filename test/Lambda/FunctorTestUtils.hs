{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lambda.FunctorTestUtils where

import Control.Monad.Reader
import Lambda.Functor
import Test.QuickCheck
import Test.QuickCheck.Checkers

-- ==========================================
-- Arbitrary and EqProp Instances
-- ==========================================

instance (Arbitrary a) => Arbitrary (MyMaybe a) where
  arbitrary =
    frequency
      [ (1, pure MyNothing),
        (3, fmap MyJust arbitrary)
      ]

instance (Eq a) => EqProp (MyMaybe a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (MaybeList a) where -- 0 length strings handled by list of non-empty strings
  arbitrary = fmap MaybeList (scale (`min` 5) arbitrary)

instance (Eq a) => EqProp (MaybeList a) where
  (=-=) :: (Eq a) => MaybeList a -> MaybeList a -> Property
  (=-=) = eq

-- ==========================================
-- Testing Helpers
-- ==========================================

-- A custom operator for "Extensional Equality" of Readers
eqReader :: (Eq a, Show a) => Reader r a -> Reader r a -> r -> Property
eqReader r1 r2 x = runReader r1 x === runReader r2 x

eqMyReader :: (Eq b, Show b) => MyReader a b -> MyReader a b -> a -> Property
eqMyReader m1 m2 x = runMyReader m1 x === runMyReader m2 x
