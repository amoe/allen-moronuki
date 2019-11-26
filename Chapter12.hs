module Chapter12 where

ch12 = "foo"

data Maybe' a = Nothing' | Just' a

ifEvenAdd2 :: Integer -> Integer
ifEvenAdd2 x = 
  if even x
  then x + 2
  else error "not even"


ifEvenAdd2' :: Integer -> Maybe Integer
ifEvenAdd2' x = 
  if even x
  then Just (x + 2)
  else Nothing

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

-- A wrapper function that does the required validation
--  This is called a 'smart constructor'.
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age 
  | name == ""  = Nothing
  | age < 0 = Nothing
  | otherwise = Just (Person name age)

data Either' a b = Left' a | Right' b

data PersonInvalid = NameEmpty | NegativeAge deriving (Eq, Show)

mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age 
  | name == ""  = Left NameEmpty
  | age < 0 = Left NegativeAge
  | otherwise = Right (Person name age)

-- Now the combined validation functions.

-- Either a list of PersonInvalid possibilties, or a real Age.
ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay x =
  if x >= 0
  then Right x
  else Left [NegativeAge]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay x =
  if x /= ""
  then Right x
  else Left [NameEmpty]

mkPerson'' :: Name -> Age -> Either [PersonInvalid] Person
mkPerson'' x y = mkPerson''' (nameOkay x) (ageOkay y)

mkPerson''' :: Either [PersonInvalid] Name -> 
               Either [PersonInvalid] Age -> 
               Either [PersonInvalid] Person
mkPerson''' (Right name) (Right age) = Right (Person name age)
mkPerson''' (Left err1) (Left err2) = Left (err1 ++ err2)
mkPerson''' (Left err1) _ = Left err1
mkPerson''' _ (Left err2) = Left err2




data Example a = Blah | Woot a deriving (Show)

-- Demonstrate that newtypes can still accept bottom values
newtype TestNewtype = TestNewtype Integer

foo :: TestNewtype -> String
foo x = "I returned"

data Identity a = Identity a


