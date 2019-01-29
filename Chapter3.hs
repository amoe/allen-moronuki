module Chapter3 where

import qualified Data.Text as T
import Network.HTTP

x :: Bool
x = True

y :: Char
y = 'a'

z :: T.Text
z = T.pack "foo"

a :: String
a = "bar"

romanNumeralTwo :: Char
romanNumeralTwo = 'Ⅱ'

main :: IO ()
main = putStrLn "hello world!"
