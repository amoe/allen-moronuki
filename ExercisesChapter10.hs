module ExercisesChapter10 where

exCh10 = 42


vizFold :: String -> String -> String -> String
vizFold op x y = concat ["(", x, op, y, ")"]

vizFoldFlip :: String -> String -> String -> String
vizFoldFlip op x y = concat ["(", y, op, x, ")"]


foldDemoXs = map show [1..5]

addFoldr = foldr (vizFold "+") "0" foldDemoXs
addFoldl = foldl (vizFold "+") "0" foldDemoXs

mulFoldr = foldr (vizFold "*") "1" foldDemoXs
mulFoldl = foldl (vizFold "*") "1" foldDemoXs


mulFoldrFlip = foldr (vizFoldFlip "*") "1" foldDemoXs
mulFoldlFlip = foldl (vizFoldFlip "*") "1" foldDemoXs


