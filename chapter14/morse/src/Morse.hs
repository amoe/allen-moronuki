module Morse (
  Morse,
  charToMorse,
  morseToChar,
  stringToMorse,
  letterToMorse,
  morseToLetter
) where

import qualified Data.Map as M

type Morse = String

letterToMorse :: M.Map Char Morse
letterToMorse = M.fromList [
  ('a', ".-"),
  ('b', "-..."),
  ('c', "-.-."),
  ('d', "-.."),
  ('e', "."),
  ('f', "..-."),
  ('g', "--."),
  ('h', "...."),
  ('i', ".."),
  ('j', ".---"),
  ('k', "-.-"),
  ('l', ".-.."),
  ('m', "--"),
  ('n', "-."),
  ('o', "---"),
  ('p', ".--."),
  ('q', "--.-"),
  ('r', ".-."),
  ('s', "..."),
  ('t', "-"),
  ('u', "..-"),
  ('v', "...-"),
  ('w', ".--"),
  ('x', "-..-"),
  ('y', "-.--"),
  ('z', "--.."),
  ('1', ".----"),
  ('2', "..---"),
  ('3', "...--"),
  ('4', "....-"),
  ('5', "....."),
  ('6', "-...."),
  ('7', "--..."),
  ('8', "---.."),
  ('9', "----."),
  ('0', "-----")
  ]


-- Invert a map.  But note that this will just silently discard multis.
morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse x = M.lookup x letterToMorse

-- Note that this returns a list of strings but in a single Maybe container.  It
-- doesn't stuff the strings together.  It just stuffs the Just values together.
-- Also this doesn't support spaces in strings.  A space will just cause the
-- lookup to fail.
stringToMorse :: String -> Maybe [Morse]
stringToMorse s = sequence $ fmap charToMorse s

morseToChar :: Morse -> Maybe Char
morseToChar x = M.lookup x morseToLetter
