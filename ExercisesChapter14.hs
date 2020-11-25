module ExercisesChapter14 where

import Test.Hspec
import WordNumber (digitToWord, getIndividualDigits, wordNumber)
import Test.QuickCheck
import Data.List (sort)

hspecMain :: IO ()
hspecMain = hspec $ do
  describe "digitToWord" $ do
    it "return zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "return one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "getIndividualDigits" $ do
    it "returns [1] for 1" $ do
      getIndividualDigits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      getIndividualDigits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
  

--   describe "QuickCheck usage" $ do
--     it "x + 1 is always greater than x" $ do
-- --      property $ \x -> (x + 1) > (x :: Int)
--       property $ f


-- half x = x / 2
-- halfIdentity = (*2) . half
-- prop_halfIdentity x = x == halfIdentity x
  
-- quickcheckMain :: IO ()
-- quickcheckMain = quickCheck prop_halfIdentity

half x = x / 2

-- defined in point free style and needs to have explicit type signature
halfIdentity :: Double -> Double
halfIdentity = (*2) . half
  
prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = halfIdentity x == x


-- How does this work?  It's a right-fold which keeps some status in a tuple
-- over xs.  The tuple is (LAST-CHAR, CURRENT-STATUS).
-- The input value 'comes in' from the first argument.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status    -- If we ever saw False, it means one pairing was wrongturn false forever.
        go y (Nothing, t) = (Just y, t)    -- If the first turn through the loop, store our last-seen-value and stay True.  [_] -> True.
        go y (Just x, t) = (Just y, x >= y) -- Compare to last-seen-value and turn False if wrongly ordered.


prop_listOrdered :: [Integer] -> Bool
prop_listOrdered xs = listOrdered $ sort xs

-- If we just type hint this properly, it automatically becomes a property.
prop_plusAssociative :: Integer -> Integer -> Integer  -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative :: Integer -> Integer -> Bool
prop_plusCommutative x y = x + y == y + x

prop_multiplyAssociative :: Integer -> Integer -> Integer  -> Bool
prop_multiplyAssociative x y z = x * (y * z) == (x * y) * z

prop_multiplyCommutative :: Integer -> Integer  -> Bool
prop_multiplyCommutative x y = x * y == y * x

-- Testing this property will fail!  Because subtraction is not associative.
prop_subtractAssociative :: Integer -> Integer -> Integer  -> Bool
prop_subtractAssociative x y z = x - (y - z) == (x - y) - z

prop_subtractCommutative :: Integer -> Integer -> Bool
prop_subtractCommutative x y = x + y == y + x

-- This will cause a divide by zero if attempted to be tested using simply the
-- regular generator for Integers.  
quotLaw :: Integer -> Integer -> Bool
quotLaw x y = (quot x y) * y + (rem x y) == x

divLaw :: Integer -> Integer -> Bool
divLaw x y = (div x y) * y + (mod x y) == x

  
-- Kind of nasty, but we don't know any other syntax to produce nonzero
-- integers.
genNonzero :: Gen Integer
genNonzero = elements nonzeroIntegers
  where nonzeroIntegers = [1..100] ++ [(-100)..(-1)]

-- Using the new Gen that we just defined in place of the standard 'arbitrary'.
genNonzeroTuple :: Gen (Integer, Integer)
genNonzeroTuple = do
  a <- genNonzero
  b <- genNonzero
  return (a, b)

-- Since we've only been shown how to get single values from generators, but we
-- need two values for one application of this function, we uncurry the quotLaw
-- so that it accepts a single 2-tuple of the form that our Gen produces.
prop_quotLaw :: Property
prop_quotLaw = forAll genNonzeroTuple $ uncurry quotLaw


prop_divLaw :: Property
prop_divLaw = forAll genNonzeroTuple $ uncurry divLaw

-- Falsifiable for 1^0 != 0^1
prop_exponentiationCommutative :: Integer -> Integer -> Bool
prop_exponentiationCommutative x y = x ^ y == y ^ x

-- Falsifiable:
-- 0 ^ (0 ^ 0) == 0  because 0 x 1 = 0
-- (0 ^ 0) ^ 0 == 1  because 1^0 = 1
prop_exponentiationAssociative :: Integer -> Integer -> Integer -> Bool
prop_exponentiationAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

  
-- Fun fact -- this is called an 'involution' mathematically.
prop_twiceReverseIdentity :: [Integer] -> Bool
prop_twiceReverseIdentity xs = (reverse . reverse) xs == xs

  
quickcheckMain :: IO ()
quickcheckMain = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multiplyAssociative
--  quickCheck prop_subtractAssociative
  quickCheck prop_subtractCommutative
  quickCheck prop_quotLaw
  quickCheck prop_divLaw
  quickCheck prop_twiceReverseIdentity
--  quickCheck prop_exponentiationCommutative
--  quickCheck prop_exponentiationAssociative
