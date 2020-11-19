module ExercisesChapter14 where

import Test.Hspec
import WordNumber (digitToWord, getIndividualDigits, wordNumber)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "return zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "return one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  
ch14 = "foo"
