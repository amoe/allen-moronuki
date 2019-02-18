module ExercisesChapter4 where

awesome = ["Papuchon", "curry", ":)"]

also = ["Quake", "The Simons"]

allAwesome :: [[String]]
allAwesome = [awesome, also]

-- davesLength :: [a] -> Integer
-- davesLength l = length l



isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = segment1 == segment2
  where mid = midpoint x
        segment1 = take mid x
        segment2 = take mid $ reverse x

midpoint :: [a] -> Int
midpoint l = div (length l) 2


-- This works but you have to call it as myAbs (-10) because literal values
-- don't work
myAbs :: Integer -> Integer
myAbs n = 
  if n > 0
    then n
  else
    negate n

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

-- Correcting syntax

x = (+)


-- This person tried to use infix syntax for prefix, but using single quotes
-- instead of backticks as required.
lengthPlusOne xs = w `x` 1
  where w = length xs

myId = \x -> x

myFst2 (a, b) = a
