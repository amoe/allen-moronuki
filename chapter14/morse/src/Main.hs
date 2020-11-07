module Main where

import Control.Monad (forever, when)
import System.Exit (exitFailure, exitSuccess)
import Data.Traversable (traverse)
import Data.List (intercalate)
import System.IO (hGetLine, hIsEOF, BufferMode(NoBuffering), hSetBuffering, stdout, stdin)
import Morse (stringToMorse, morseToChar, charToMorse)

convertToMorse :: IO ()
convertToMorse = forever $ do
  isDone <- hIsEOF stdin
  when isDone exitSuccess
  line <- hGetLine stdin
  convertLine line
  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just x) -> putStrLn (intercalate " " x)
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  isDone <- hIsEOF stdin
  when isDone exitSuccess
  line <- hGetLine stdin
  convertLine line

  where
    convertLine line = do
      -- Words breaks up the string into morse values.
      let val = traverse morseToChar (words line)
      case val of
        (Just x) -> putStrLn x
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure
  
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please type some input: "
  input <- getLine
  putStrLn "Hello, world!"
  putStrLn $ case (stringToMorse input) of
    (Just x) -> concat x
    Nothing -> "INVALID"
