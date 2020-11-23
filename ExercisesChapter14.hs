module ExercisesChapter14 where

import Test.Hspec
import WordNumber (digitToWord, getIndividualDigits, wordNumber)
import Test.QuickCheck  

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
prop_halfIdentity x = x == x

quickcheckMain :: IO ()
quickcheckMain = quickCheck prop_halfIdentity
