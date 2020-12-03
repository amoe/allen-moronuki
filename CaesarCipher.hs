module CaesarCipher (modularCaesar, modularUncaesar) where

import Data.Char (ord, chr)

data CharacterRange = CharacterRange Int Int deriving (Eq, Show)

lcRange = CharacterRange lcBase ((ord 'z' - lcBase) + 1)
  where lcBase = ord 'a'

ucRange = CharacterRange ucBase ((ord 'Z' - ucBase) + 1)
  where ucBase = ord 'A'
  
allRanges = [lcRange, ucRange]

-- XXX, for now we don't care about the case where we are not within the specified range.
-- Already taken care of above.  
shiftByRange :: Int -> Char -> CharacterRange -> Char
shiftByRange n x (CharacterRange base size) = chr $ base + (mod newPosition size)
  where relativePosition = (ord x) - base
        newPosition = relativePosition + n

inRange :: Char -> CharacterRange -> Bool
inRange c (CharacterRange a n) = val >= a && val < a + n
  where val = ord c

findRange :: [CharacterRange] -> Char -> Maybe CharacterRange
findRange xs c = foldr (\x y -> if inRange c x then Just x else y) Nothing xs
  

maybeShiftUpModular :: Int -> Char -> Char
maybeShiftUpModular n c = case findRange allRanges c of
 (Just cr) -> shiftByRange n c cr
 Nothing -> c
  
modularCaesar :: Int -> [Char] -> [Char]
modularCaesar n xs = map (maybeShiftUpModular n) xs        

modularUncaesar :: Int -> [Char] -> [Char]
modularUncaesar n xs = modularCaesar (negate n) xs
  
