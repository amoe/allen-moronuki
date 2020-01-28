module Chapter13 where

import qualified Data.Bool
import qualified Data.Bool as B
import Data.List (intersperse)
import System.Random (randomRIO)

ch13 = 42

concatUserInput = do
  x1 <- getLine
  x2 <- getLine
  return (x1 ++ x2)

twoo :: IO Bool
twoo = do 
  c <- getChar
  c' <- getChar
  return (c == c')

-- This is going to do IO to grab the two chars and then either return true or
-- nothing.
twoo' :: IO ()
twoo' = do 
  c <- getChar
  c' <- getChar
  if c == c'
  then putStrLn "True"
  else return ()

-- We can re-bind the type signature for the `all` function to anything with
-- a Foldable instance.

myAll :: (a -> Bool) -> [a] -> Bool
myAll = all

myAll' :: (a -> Bool) -> Maybe a -> Bool
myAll' = all

myAll'' :: (a -> Bool) -> Either b a -> Bool
myAll'' = all

