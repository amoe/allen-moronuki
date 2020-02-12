module Addition where

import Test.Hspec

dividedBy :: (Ord a, Num a) => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)



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




