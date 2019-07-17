module ExercisesChapter8 where

foo = 42

func :: [a] -> [a] -> [a]
func x y = x ++ y

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"

blah = cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

-- Recursion, ex 2.  Sum all numbers from 1 to n
sum1ToN :: (Eq a, Num a) => a -> a
sum1ToN 1 = 1
sum1ToN n = n + (sum1ToN (n - 1))

iteratedMultiply :: (Integral a) => a -> a -> a
iteratedMultiply x y = go x y 0
  where go x y count
          | count == (y - 1) = x
          | otherwise = go (x + y) y (count + 1)
