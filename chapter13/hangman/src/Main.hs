module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

type WordList = [String]

minWordLength = 5
maxWordLength = 9

validWord :: String -> Bool
validWord s = c > minWordLength && c < maxWordLength
  where c = length s

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ lines dict

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return $ filter validWord aw

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

-- Not sure what's happening here!  Basically pass gameWords as an argument
-- to randomWord, removing the IO when passing, but keeping the IO on return.
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- Second member is the list of characters that we have correctly guessed.

-- Third member is an ordered list of characters that we already submitted as
-- guesses, that may or may not be correct.

data Puzzle = Puzzle String [Maybe Char] [Char]

-- is fmap necessary here? 
instance Show Puzzle where
  show (Puzzle _ discovered guessed) = 
    discoveredString ++ " Guessed so far: " ++ guessed
    where discoveredString = intersperse ' ' $ fmap renderPuzzleChar discovered

-- Could probably also be done as one of the folds.
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just x) = x

freshPuzzle :: String -> Puzzle
freshPuzzle x = Puzzle x blanks []
  where blanks = map (const Nothing) x

main :: IO ()
main = do
  putStrLn "Hello, world!"
  return ()

