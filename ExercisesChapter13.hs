module ExercisesChapter13 where

import Data.Char (isUpper, toUpper, ord, chr, toLower)
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

palindromeWhitelist = ['a'..'z']

validPalindromeChar :: Char -> Bool
validPalindromeChar c = elem c palindromeWhitelist

stripInvalid :: String -> String
stripInvalid s = filter validPalindromeChar s

massageString :: String -> String
massageString s = stripInvalid $ map toLower s

isPalindrome :: String -> Bool
isPalindrome s = s' == reverse s'
  where s' = massageString s

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case line1 == reverse line1 of
    True -> putStrLn "It is a palindrome."
    False -> do
      putStrLn "Bad luck; it was not a palindrome."
      exitSuccess

-- This version uses the more forgiving way to determine if a string is
-- a palindrome.
palindrome' :: IO ()
palindrome' = forever $ do
  line1 <- getLine
  case isPalindrome line1 of
    True -> putStrLn "It is a palindrome."
    False -> do
      putStrLn "Bad luck; it was not a palindrome."
      exitSuccess

-- "Modifying code", Ex. 4

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show


data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = 
    Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++ "; Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please enter a name."
  personName <- getLine
  putStrLn "Please enter an age."
  personAgeString <- getLine
  let personAge = read personAgeString :: Integer

  case mkPerson personName personAge of
    (Left x) -> do
      putStrLn "An error occurred."
      putStrLn $ "The error was: " ++ show x
    (Right x) -> do
      putStrLn "It worked!"
      putStrLn $ "The Person is: " ++ show x

  return ()
