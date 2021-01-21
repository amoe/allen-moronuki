module ExercisesChapter15 where

import Data.Monoid
import Test.QuickCheck

-- This optional monoidally combines its contained values in the case where both
-- are present, rather than just preferring a certain side, as `First` and `Last`
-- would do.
data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only x) (Only y) = Only (x <> y)
  (<>) (Only x) Nada = Only x
  (<>) Nada (Only x) = Only x
  (<>) Nada Nada = Nada

instance Monoid a => Monoid (Optional a) where
  -- Combining with the identity should lead to the non-identity value being
  -- preserved
  mempty = Nada
  mappend x y = x <> y

onlySum = Only (Sum 1)  
val1 = onlySum `mappend` onlySum

onlyFour = Only (Product 4)
onlyTwo = Only (Product 2)
val2 = onlyFour `mappend` onlyTwo

val3 = Only (Sum 1) `mappend` Nada

val4 = Only [1] `mappend` Nada

val5 = Nada `mappend` Only (Sum 1)  


newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' (Only x)) _ = First' (Only x)
  (<>) _ (First' (Only x)) = First' (Only x)
  (<>) _ _ = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada


isMonoidAssociative :: (Eq m, Monoid m) => m -> m -> m -> Bool
isMonoidAssociative a b c = a <> (b <> c) == (a <> b) <> c

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = (x <> mempty) == x

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = (mempty <> x) == x

-- See MaybeAnotherMonoid.hs for the implementation of that exercise.

-- Semigroup exercises.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

isSemigroupAssociative :: (Eq m, Semigroup m) => m -> m -> m -> Bool
isSemigroupAssociative a b c = a <> (b <> c) == (a <> b) <> c

  
qcMain :: IO ()
qcMain = do
  quickCheck (isSemigroupAssociative :: Trivial -> Trivial -> Trivial -> Bool)
