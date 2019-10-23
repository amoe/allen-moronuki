module VigenereCipher where

import Data.Char (ord, chr)

keyword = ['A', 'L', 'L', 'Y']
message = ['M', 'E', 'E', 'T', 'A', 'T', 'D', 'A', 'W', 'N']


-- These functions are the same as they were in the Caesar cipher.
shiftUpModular :: Int -> Char -> Char
shiftUpModular n x = chr $ base + (mod newPosition 26)
  where base = ord 'A'
        relativePosition = (ord x) - base
        newPosition = relativePosition + n

shiftDownModular :: Int -> Char -> Char
shiftDownModular n x = chr $ base + (mod newPosition 26)
  where base = ord 'A'
        relativePosition = (ord x) - base
        newPosition = relativePosition - n


-- An infinite list [a] is still denoted by the type [a].

determineShift k = (ord k) - alphabetBase
  where alphabetBase = ord 'A'


-- Encoder 
encodeTuple (k, p)  = shiftUpModular shift p
   where shift = determineShift k
encodeText keyword message = map encodeTuple $ generatePairs keyword message

-- Decoder
decodeTuple (k, p)  = shiftDownModular shift p
   where shift = determineShift k
decodeText keyword message = map decodeTuple $ generatePairs keyword message


-- Create an infinite list of the keyword
generatePairs :: [a] -> [b] -> [(a, b)]
generatePairs keyword message = zip loopedKey message
  where loopedKey = keyword ++ loopedKey

