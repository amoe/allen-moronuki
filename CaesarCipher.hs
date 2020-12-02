module CaesarCipher (modularCaesar, modularUncaesar) where

import Data.Char (ord, chr)

-- Caesar cipher

lowercaseStart = ord 'a'
lowercaseEnd = ord 'z'
lowercaseRangeSize = lowercaseEnd - lowercaseStart   -- XXX: 25??
  
uppercaseStart = ord 'A'
uppercaseEnd = ord 'Z'
uppercaseRangeSize = uppercaseEnd - uppercaseStart

-- shiftByRange :: Int -> Char -> Int -> Int -> Char

data CharacterRange = CharacterRange Int Int deriving (Eq, Show)

lcRange = CharacterRange lcBase ((ord 'z' - lcBase) + 1)
  where lcBase = ord 'a'

ucRange = CharacterRange ucBase ((ord 'Z' - ucBase) + 1)
  where ucBase = ord 'A'
  
allRanges = [lcRange, ucRange]

-- XXX, for now we don't care about the case where we are not within the specified range.  
shiftByRange :: Int -> Char -> CharacterRange -> Char
shiftByRange n x (CharacterRange base size) = chr $ base + (mod newPosition size)
  where relativePosition = (ord x) - base
        newPosition = relativePosition + n
  
-- maybeShiftUpModular :: Int -> Char -> Char
-- maybeShiftUpModular n x
--   | isInLowercaseRange x = shiftByRange
--   ẅhere y = ord 'x'

  

  
shiftUpModular :: Int -> Char -> Char
shiftUpModular n x = chr $ base + (mod newPosition 26)
  where base = ord 'a'
        relativePosition = (ord x) - base
        newPosition = relativePosition + n

shiftDownModular :: Int -> Char -> Char
shiftDownModular n x = chr $ base + (mod newPosition 26)
  where base = ord 'a'
        relativePosition = (ord x) - base
        newPosition = relativePosition - n

modularCaesar :: Int -> [Char] -> [Char]
modularCaesar n xs = map (shiftUpModular n) xs        

modularUncaesar :: Int -> [Char] -> [Char]
modularUncaesar n xs = map (shiftDownModular n) xs        
  
