module MaybeAnotherMonoid where

-- Solution to ch15 this is a bit complicated.

import Data.Monoid
import Test.QuickCheck


data Optional a = Nada | Only a deriving (Eq, Show)

-- Use record syntax to define an accessor for the first field
newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

-- Monoid properties

isMonoidAssociative :: (Eq m, Monoid m) => m -> m -> m -> Bool
isMonoidAssociative a b c = a <> (b <> c) == (a <> b) <> c

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = (x <> mempty) == x

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = (mempty <> x) == x


instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return (First' Nada)), (1, return (First' (Only a)))]


instance Semigroup (First' a) where
  (<>) (First' (Only x)) _ = First' (Only x)
  (<>) _ (First' (Only x)) = First' (Only x)
  (<>) _ _ = First' Nada


instance Monoid (First' a) where
  mempty = First' Nada

main = do
  quickCheck (isMonoidAssociative :: First' String -> First' String -> First' String -> Bool)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
