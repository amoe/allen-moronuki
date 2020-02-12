module Addition where

import Test.Hspec

dividedBy :: a -> a -> (a, a)
dividedBy x y = (x,y)

main :: IO ()
main = hspec $ do
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      (dividedBy 15 3) `shouldBe` (5,0)
    it "22 divided by 5 is (4 remainder 2)" $ do
      (dividedBy 22 5) `shouldBe` (4,2)



