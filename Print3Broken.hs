module Print3Broken where

-- not broken anymore!

greeting :: String
greeting = "Yarrrr"

printSecond :: IO ()
printSecond = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond
  
