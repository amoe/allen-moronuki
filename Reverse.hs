module Reverse where

rvrs :: String -> String
rvrs x = (drop 9 x) ++ middle ++ (take 5 x)
  where middle = take 4 (drop 5 x)

main :: IO ()
main = print $ rvrs "Curry is awesome"
