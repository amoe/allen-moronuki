module Main where

import DogsRule
import Hello
import System.IO

characters = ["fry", "leela", "bender"]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please enter your name: "
  name <- getLine
  sayHello name
  dogs

