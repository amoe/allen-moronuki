module Spec where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

allowedChars :: [Char]
allowedChars = M.keys letterToMorse
  
allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse


-- f :: (Show a, Testable prop) => a -> prop
-- f c = ((charToMorse c) >>= morseToChar) == Just c

-- Why does this use the monadic bind operator?  Because charToMorse results in
-- a `Maybe Morse` -- i.e. a (Just String) or Nothing.
f :: Char -> Bool
f c = ((charToMorse c) >>= morseToChar) == Just c

h :: Char -> Maybe Char  
h x = (charToMorse x) >>= morseToChar

g :: String -> Maybe Char
g _ = Just '!'

  
prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen (\c -> ((charToMorse c) >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_thereAndBackAgain

  
