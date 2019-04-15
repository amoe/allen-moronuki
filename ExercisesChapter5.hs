{-# LANGUAGE NoMonomorphismRestriction #-}

module ExercisesChapter5 where

x = 42

-- Does it compile?

-- 1, fixed version:
bigNum = (^) 5
wahoo = bigNum $ 10

