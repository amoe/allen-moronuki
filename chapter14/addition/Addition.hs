module Addition where

import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "2 + 2 is equal to 4" $ do
      (2 + 2) == 4 `shouldBe` True
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True

