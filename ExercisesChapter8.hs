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

-- Handles the case where both are negative -- yes.
dividedByBothNegative :: Integral a => a -> a -> (a, a)
dividedByBothNegative x y = go x y 0
  where go n denom count
          | (abs n) < (abs denom) = (count, n)
          | otherwise = go (n - denom) denom (count + 1)


-- You can't just keep subtracting, because you're in fact adding, so you
-- never have any limit.  You'll just have to invert the denom.
dividedByNegDenom :: Integral a => a -> a -> (a, a)
dividedByNegDenom x y = go x (-y) 0
  where go n denom count
          | n < denom = (-count, n)
          | otherwise = go (n - denom) denom (count + 1)

-- Handles both negative and positive denom.
dividedByNegDenom' :: Integral a => a -> a -> (a, a)
dividedByNegDenom' x y = go x (abs y) 0
  where go n denom count
          | n < count = ((signum y) * count, n)
          | otherwise = go (n - denom) denom (count + 1)

-- This final version works for all cases
dividedByAllCases :: Integral a => a -> a -> (a, a)
dividedByAllCases x y = go (abs x) (abs y) 0
  where go n denom count
          | n < count = ((signum x) * (signum y) * count, n)
          | otherwise = go (n - denom) denom (count + 1)


