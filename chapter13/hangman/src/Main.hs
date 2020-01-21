module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

main :: IO ()
main = do
  putStrLn "Hello, world!"
  return ()

