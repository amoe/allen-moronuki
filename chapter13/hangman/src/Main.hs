module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

--type WordList = [String]

newtype WordList = WordList [String] deriving (Eq, Show)


minWordLength = 5
maxWordLength = 9

validWord :: String -> Bool
validWord s = c > minWordLength && c < maxWordLength
  where c = length s

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList $ filter validWord aw

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
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

-- Returns true if the guess is correct.
charInWord :: Char -> Puzzle -> Bool
charInWord x (Puzzle x' _ _) = elem x x'

alreadyGuessed :: Char -> Puzzle -> Bool
alreadyGuessed x (Puzzle _ _ x') = elem x x'

-- Insert a correctly guessed character into the string.
-- l designates the correctly guessed letter.
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle x y z) l = Puzzle x y' z'
  where y' = map (flipIfCorrect l) $ zip x y
        z' = (l : z)

flipIfCorrect :: Char -> (Char, Maybe Char) -> Maybe Char
flipIfCorrect guessed (real, currentStatus) = if guessed == real
                                              then (Just real)
                                              else currentStatus

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do 
  putStrLn $ "Your guess was: " ++ [guess]

  -- Interesting control structure here
  -- WTF is happening here?  What are these returns doing
  -- Presumably they are not exiting the entire outer 'do' block?
  -- WTF, they actually do?  How is that defined?
  -- Actually no, they just exit the local do block.  And because the result
  -- is an `IO Puzzle` already, it satisfies the type signature.
  case (charInWord guess puzzle, alreadyGuessed guess puzzle) of
    (_, True) -> do
      putStrLn "Duplicate guess, bozo."
      return puzzle
    (True, _) -> do
      putStrLn "You Guessed Correctly."
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "The guess was wrong."
      return (fillInCharacter puzzle guess)
    

-- Print a rude message if they failed, otherwise do absolutely nothing.
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) = 
  if (length guessed) > 7
  then do
    putStrLn "YOU LOSE!"
    putStrLn ("The word was: " ++ wordToGuess)
    exitSuccess
  else
    return ()


winCondition :: Puzzle -> Bool
winCondition (Puzzle wordToGuess discovered guessed) = all isJust discovered

gameWin :: Puzzle -> IO ()
gameWin puzzle = do
  if (winCondition puzzle)
    then do
    putStrLn "YOU WIN!"
    exitSuccess
    else  return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "

  guess <- getLine
  
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Only single-character guesses are allowed."


testPuzzle = freshPuzzle "turbulent"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'

  let puzzle = freshPuzzle (map toLower word)
  runGame puzzle

