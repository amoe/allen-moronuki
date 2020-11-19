module Chapter14 where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)
import Test.Hspec

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-- Product types
data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  x <- arbitrary
  y <- arbitrary
  return (Pair x y)

  

pairGenInt :: Gen (Pair Int Int)
pairGenInt = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

-- Sum types

data Sum a b = First a | Second b deriving (Eq, Show)

-- Enumerate the possibilities and pass to oneof which will just select.
--
-- It's called 'equal' because each value of the sum type has an equal
-- probability.
sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual


-- Use a weighted frequency distribution to get certain sum types more often
-- than others.
sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a),
             (1, return $ Second b)]


sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

