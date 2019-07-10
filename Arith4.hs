module Arith4 where

-- Demonstrating round-trip serialization using Read and Show.

roundTrip :: (Show a, Read a) => a -> a
roundTrip x = read $ show x

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 x = read $ show x

main = do
  print $ roundTrip 4
  print $ roundTripPF 4
  print $ roundTrip2 4
  print $ id 4
