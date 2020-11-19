module ExercisesChapter14 where

import Test.Hspec
import WordNumber (digitToWord, getIndividualDigits, wordNumber)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "return zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
  
ch14 = "foo"
