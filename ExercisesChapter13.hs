module ExercisesChapter13 where

import Data.Char (isUpper, toUpper, ord, chr)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

-- Copies of the code from the exercises from chapter 9.

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
