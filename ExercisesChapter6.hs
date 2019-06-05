module ExercisesChapter6 where

x = 42

-- Does not type check -- no Show instance on Person.
-- data Person = Person Bool
-- printPerson :: Person -> IO ()
-- printPerson person = putStrLn (show person)

-- Does not type check -- no Eq instance on Mood.
-- data Mood = Blah | Woot deriving Show
-- settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- Won't type check.  Need explicit type coercion.
--phew = Papu "chases" True

-- Type checks and yields a partially-applied data constructor.
truth = Papu (Rocks "chomskydoz")

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

