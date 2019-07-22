module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "invalid digit"
    
-- digits :: Int -> [Int]
-- digits n = []
--   where magnitude = numberOfDigitsInInteger n

-- Need to do the conversion using fromInteger
getSeries :: Integer -> [Integer]
getSeries n = take (fromInteger nDigits) $ enumFrom 1
  where nDigits = numberOfDigitsInInteger n



wordNumber :: Int -> String
wordNumber n = undefined

-- Head recursive version
numberOfDigitsInInteger :: Integer -> Integer
numberOfDigitsInInteger 0 = 0
numberOfDigitsInInteger n = 1 + numberOfDigitsInInteger (div n 10)

-- Something like this needs to happen:
-- But a version that will actually type check
-- getCertainDigit :: Integral a => Integer -> a -> a
-- getCertainDigit n x = d
--   where wantedPower = doExpt 10 n
--         (x', _) = divMod x wantedPower
--         d = mod x' 10


-- Integer exponentiation
doExpt :: Integer -> Integer -> Integer
doExpt _ 0 = 1
doExpt x y = x * (doExpt x (y - 1))
