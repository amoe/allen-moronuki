module ExercisesChapter3 where

-- Building functions

-- a)
daveFunction1 s = s ++ "!"

-- b)
-- We have to cons onto an empty list to stringify the result of `!!`.
daveFunction2 :: [Char] -> [Char]
daveFunction2 s = (s !! 4) : ""

-- c)
daveFunction3 :: [Char] -> [Char]
daveFunction3 s = drop 9 s

-- 3

thirdCharacter :: String -> Char
thirdCharacter s = s !! 2

-- 4

letterIndex :: Int -> Char
letterIndex n = "Curry is awesome!" !! (n - 1)

-- 5


rvrs :: String
rvrs = (drop 9 input) ++ middle ++ (take 5 input)
  where input = "Curry is awesome"
        middle = take 2 (drop 6 input)

