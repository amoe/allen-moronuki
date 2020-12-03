module VigenereCipher (encodeText, decodeText) where

import Data.Char (ord, chr)

keyword = ['A', 'L', 'L', 'Y']
message = ['M', 'E', 'E', 'T', 'A', 'T', 'D', 'A', 'W', 'N']


data CharacterRange = CharacterRange Int Int deriving (Eq, Show)

lcRange = CharacterRange lcBase ((ord 'z' - lcBase) + 1)
  where lcBase = ord 'a'

ucRange = CharacterRange ucBase ((ord 'Z' - ucBase) + 1)
  where ucBase = ord 'A'
  
allRanges = [lcRange, ucRange]

shiftByRange :: Int -> Char -> CharacterRange -> Char
shiftByRange n x (CharacterRange base size) = chr $ base + (mod newPosition size)
  where relativePosition = (ord x) - base
        newPosition = relativePosition + n

inRange :: Char -> CharacterRange -> Bool
inRange c (CharacterRange a n) = val >= a && val < a + n
  where val = ord c

findRange :: [CharacterRange] -> Char -> Maybe CharacterRange
findRange xs c = foldr (\x y -> if inRange c x then Just x else y) Nothing xs

-- An infinite list [a] is still denoted by the type [a].

determineShift :: Char -> CharacterRange -> Int  
determineShift k (CharacterRange a n) = (ord k) - a

-- Encoder
encodeTuple (k, p)  = case (findRange allRanges p) of
  (Just cr) -> shiftByRange (determineShift k cr) p cr
  Nothing -> p
  
encodeText keyword message = map encodeTuple $ generatePairs keyword message

-- Decoder
  
decodeTuple (k, p)  = case (findRange allRanges p) of
  (Just cr) -> shiftByRange (negate (determineShift k cr)) p cr
  Nothing -> p
  
decodeText keyword message = map decodeTuple $ generatePairs keyword message

-- Create an infinite list of the keyword
generatePairs :: [a] -> [b] -> [(a, b)]
generatePairs keyword message = zip loopedKey message
  where loopedKey = keyword ++ loopedKey
