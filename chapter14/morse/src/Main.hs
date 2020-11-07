module Main where

import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import Morse (stringToMorse, morseToChar, charToMorse)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please type some input: "
  input <- getLine
  putStrLn "Hello, world!"
  putStrLn $ case (stringToMorse input) of
    (Just x) -> concat x
    Nothing -> "INVALID"
