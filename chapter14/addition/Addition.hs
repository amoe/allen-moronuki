module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: (Ord a, Num a) => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)


multipliedBy :: (Eq a, Num a) => a -> a -> a
multipliedBy x y = go x y 0
  where go x y count
          | count == (y - 1) = x
          | otherwise = go (x + y) y (count + 1)

f :: Int -> Bool
f x = (x + 1) > x

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "2 + 2 is equal to 4" $ do
      (2 + 2) == 4 `shouldBe` True
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      (dividedBy 15 3) `shouldBe` (5,0)
    it "22 divided by 5 is (4 remainder 2)" $ do
      (dividedBy 22 5) `shouldBe` (4,2)
  describe "Trivial rules" $ do
    it "is sane" $ do
      True `shouldBe` True
  describe "Multiplication" $ do
    it "3 by 3 is 9" $ do
      (multipliedBy 3 3) `shouldBe` 9    
    it "3 by 1 is 3" $ do
      (multipliedBy 3 1) `shouldBe` 3
  describe "QuickCheck usage" $ do
    it "x + 1 is always greater than x" $ do
--      property $ \x -> (x + 1) > (x :: Int)
      property $ f

-- This will not work because arbitrary has an ambiguous type in this context.
--ch14 = sample arbitrary

-- Now, trivialInt is going to always return 1.
trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

-- Now, the value one has a higher probability of showing up in our selected
-- set.
oneThroughThreeNonUniform :: Gen Int
oneThroughThreeNonUniform = elements [1,2,3,1]

-- These two Bool ones look identical.
genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

