module RandomUser where

import System.Random

range :: (Integer, Integer)
range = (1, 5)

main :: IO ()
main = do
  x <- randomRIO range
  putStrLn $ show x
  putStrLn "Hello, world!"
