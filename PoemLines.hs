module PoemLines where

x = 42

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen
  ++ thirdSen ++ fourthSen


isSpace = (== '\n')
isNonSpace = (/= '\n')

myLines :: String -> [String]
myLines "" = []
myLines s = (:) word (myLines rest)
  where trimmed = dropWhile isSpace s
        word = takeWhile isNonSpace trimmed
        rest = dropWhile isNonSpace trimmed

shouldEqual = [ "Tyger Tyger, burning bright"
              , "In the forests of the night"
              , "What immortal hand or eye"
              , "Could frame thy fearful symmetry?"
              ]

main :: IO ()
main = 
  print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
