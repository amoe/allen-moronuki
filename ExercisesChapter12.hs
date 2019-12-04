module ExercisesChapter12 where

import Data.List (intersperse)

ch12 = "foo"

modifyWord :: String -> String
modifyWord x = case notThe x of
  (Just x) -> x
  Nothing -> "a"


-- Intersperse has been introduced at this stage.
replaceThe :: String -> String
replaceThe x = concat $ intersperse " " $ map modifyWord w
  where w = words x

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x
