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


morseToLetter :: M.Map Morse Char
morseToLetter = undefined


charToMorse :: Char -> Maybe Morse
charToMorse x = M.lookup x letterToMorse


morseToChar = undefined
stringToMorse = undefined
