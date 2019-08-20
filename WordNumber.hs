module WordNumber where

-- Difficult exercise from Chapter 8!

import Data.List (intersperse)

digitToWord :: Integer -> String
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
    
-- Need to do the conversion using fromInteger
getSeries :: Integer -> [Integer]
getSeries n = take (fromInteger nDigits) $ enumFrom 1
  where nDigits = numberOfDigitsInInteger n

getIndividualDigits :: Integer -> [Integer]
getIndividualDigits n = reverse $ map (getCertainDigit n) (getSeries n)

wordNumber :: Integer -> String
wordNumber n = concat (intersperse "-" (map digitToWord (getIndividualDigits n)))

-- Head recursive version
numberOfDigitsInInteger :: Integer -> Integer
numberOfDigitsInInteger 0 = 0
numberOfDigitsInInteger n = 1 + numberOfDigitsInInteger (div n 10)

-- Something like this needs to happen:
-- But a version that will actually type check
getCertainDigit :: Integral a => a -> a -> a
getCertainDigit x n = d
  where wantedPower = doExpt 10 (n - 1)
        (x', _) = divMod x wantedPower
        d = mod x' 10

-- Integer exponentiation (head recursive)
doExpt :: Integral a => a -> a -> a
doExpt _ 0 = 1
doExpt x y = x * (doExpt x (y - 1))
