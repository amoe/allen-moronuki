module ExercisesChapter13 where

import Data.Char (isUpper, toUpper, ord, chr)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import System.Exit (exitSuccess)
import Control.Monad

-- Copies of the code from the exercises from chapter 9.
-- Caesar cipher

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

-- We can't read the key yet, because we don't know how to convert from
-- String to Int.

-- Just for ease of testing.
defaultShift = 1

interactiveCaesar :: IO ()
interactiveCaesar = do
  hSetBuffering stdout NoBuffering
  putStr "Enter the text to be encrypted: "
  theInput <- getLine
  putStr "The encrypted text is: "
  putStrLn $ modularCaesar defaultShift theInput
  return ()

interactiveNumber :: IO ()
interactiveNumber = do
  putStrLn "Enter a number."
  theInput <- getLine
  let inputNumber = (read theInput :: Integer)
  putStrLn ("Input was " ++ (show (inputNumber * 2)))
  return ()

-- The vigenere cipher only works with uppercase, for whatever reason.

keyword = ['A', 'L', 'L', 'Y']
message = ['M', 'E', 'E', 'T', 'A', 'T', 'D', 'A', 'W', 'N']

shiftUpModular' :: Int -> Char -> Char
shiftUpModular' n x = chr $ base + (mod newPosition 26)
  where base = ord 'A'
        relativePosition = (ord x) - base
        newPosition = relativePosition + n

shiftDownModular' :: Int -> Char -> Char
shiftDownModular' n x = chr $ base + (mod newPosition 26)
  where base = ord 'A'
        relativePosition = (ord x) - base
        newPosition = relativePosition - n

determineShift k = (ord k) - alphabetBase
  where alphabetBase = ord 'A'

encodeTuple (k, p)  = shiftUpModular' shift p
   where shift = determineShift k
encodeText keyword message = map encodeTuple $ generatePairs keyword message

decodeTuple (k, p)  = shiftDownModular' shift p
   where shift = determineShift k
decodeText keyword message = map decodeTuple $ generatePairs keyword message


generatePairs :: [a] -> [b] -> [(a, b)]
generatePairs keyword message = zip loopedKey message
  where loopedKey = keyword ++ loopedKey

interactiveVigenere :: IO ()
interactiveVigenere = do
  putStrLn "Enter a keyword (any values are fine)."
  keyword <- getLine
  putStrLn "Enter a message (it must be in all upper case)."
  message <- getLine

  let cipherText = encodeText keyword message

  putStrLn ("The cipher-text is: " ++ cipherText)


palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case line1 == reverse line1 of
    True -> putStrLn "It is a palindrome."
    False -> do
      putStrLn "Bad luck; it was not a palindrome."
      exitSuccess
