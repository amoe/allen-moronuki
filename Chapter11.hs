module Chapter11 where

import Data.Int (Int8)

ch11 = 42

data MyBool = MyFalse | MyTrue

data Trivial = Trivial'

data UnaryTypeCon a = UnaryValueCon a


-- Data constructors and values

-- PugType is a type constant, PugData is a constant value.
data PugType = PugData

-- Phantom type variable.  It does not appear in any data constructor.
data HuskyType a = HuskyData

-- Type variables do not need to have single character names.
data DogueDeBordeaux doge = DogueDeBordeaux doge

-- The type hint is not technically needed here.
myPug = PugData :: PugType

-- Or here tbh
myHusky :: HuskyType a
myHusky = HuskyData

-- Put a type class constraint on this husky.  It holds a Num.
-- Despite not having a 'witness' for its type variable a.
myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)


-- What's a type and what's data?

-- The type 'Integer' is a "type argument" to the DATA constructor.
-- Because the type Integer is fixed at compile time, there are no type variables.
-- Therefore the type constructor `Price` can be called a "type constant".
data Price = Price Integer deriving (Eq, Show)

-- A sum type with 3 data constructors.  No arguments.
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)


-- Same
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
  deriving (Eq, Show)

-- Here, Car takes 2 type arguments (Manufacturer and Price).
-- Plane takes one type argument (Airline).
data Vehicle = Car Manufacturer Price | Plane Airline


-- It's important to realize that some stuff is being GENERATED: Car and Plane
-- are generated; in contrast, the type arguments to the data constructors
-- (Manufacturer, Price, Airline) must ALREADY EXIST in the compilation context.

-- In a way, it's too late to get this lesson so sharply, because I already
-- realized it.


-- Arities of data constructors

-- Nullary
data Example0 = Example0 deriving (Eq, Show)

-- Unary data constructor
data Example1 = Example1 Int deriving (Eq, Show)

-- Binary data constructor and therefore also a product type.
data Example2 = Example2 Int String deriving (Eq, Show)

-- A unary data constructor (MyVal)
data MyType = MyVal Int deriving (Eq, Show)

-- The type of MyVal is `Int -> MyType`.  There is 1 arrow, therefore there is
-- 1 argument.

-- Calculate the cardinality, but because we overflow the Int8 type we must use
-- toInteger to up-cast at runtime.
cardinalityOfInt8 :: Integer
cardinalityOfInt8 = (negate arche) + 1 + telos
  where arche = toInteger $ (minBound :: Int8)
        telos = toInteger $ (maxBound :: Int8)

cardinalityOfInt :: Integer
cardinalityOfInt = (negate arche) + 1 + telos
  where arche = toInteger $ (minBound :: Int)
        telos = toInteger $ (maxBound :: Int)


-- The value produced by the data constructor `MakeExample` inhabits the type
-- `Example`.  The cardinality of the type `Example` is 1.
data Example = MakeExample deriving (Show);

-- data Goats = Goats Int deriving (Eq, Show);


-- 43 goats is too many.
tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

-- Declare a wrapper type for Goats.  It might be better called GoatLimitation?
tooManyGoats' :: Goats -> Bool
tooManyGoats' (Goats n) = n > 42  

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- Despite really being an Int, we can still define different type class instances
-- for goats.
instance TooMany Goats where
  tooMany (Goats n) = n > 64
  

type Goats' = Int

-- Won't work
-- instance TooMany Goats' where
--   tooMany n = n > 64


-- A type which clearly has cardinality 3.

data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)


-- This type:
-- (*) 3 3
-- Hence has cardinality 6.
data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)

-- Same cardinality.  This just creates a type but doesn't provide any means
-- to create data that's OF the type.
type TwoQsTpl = (QuantumBool, QuantumBool)


-- A simple product type
-- But its cardinality is basically infinite.
data Person = MkPerson String Int

jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson x _) = x


data Person' = 
  Person' { name :: String
          , age :: Int }
  deriving (Eq, Show)

-- Defining two special types.  Doing this means that we can match directly
-- on the type.  eg you can't write a type signature to match only True.
-- (although you can write such a pattern match.)

-- Cardinality 1
data Fiction = MkFiction deriving Show
-- Cardinality 1
data Nonfiction = MkNonfiction deriving Show

-- Cardinality = 2
-- Data constructors are parameterized by the type constructors of the previous
-- definitions.
data BookType = FictionBook Fiction | NonfictionBook Nonfiction 
  deriving Show

type AuthorName = String

-- Product type but is it?
-- "Not in normal form as not a sum of products"
data Author = Author (AuthorName, BookType) deriving (Show)

-- Applying the distributive property
-- a*(b+c) = ab + ac
-- We have created two data constructors 
data Author' = Fiction AuthorName | Nonfiction AuthorName deriving (Eq, Show)


-- In normal form because it's a sum of products.
-- data Expr = Number Int | Add Expr Expr | Minus Expr | Mult Expr Expr | Divide Expr Expr

-- Stricter interpretation of normal form.
-- type Number = Int
-- type Add = (Expr, Expr)
-- type Minus = Expr
-- type Mult = (Expr, Expr)
-- type Divide = (Expr, Expr)

-- Can't declare this because it is a cycle for some reason?
-- type Expr = Either Number (Either Add (Either Minus (Either Mult Divide)))














































-- Type with a nullary data constructor
data GuessWhat = Chickenbutt deriving (Eq, Show)

-- A type with a unary data constructor
data Id a = MkId a deriving (Eq, Show)

-- A product type
data Product a b = Product a b deriving (Eq, Show)


-- A sum type with two type arguments
data Sum a b = First a | Second b deriving (Eq, Show)


data RecordProduct a b = 
  RecordProduct { pfirst :: a
                , psecond :: b }
  deriving (Eq, Show)


newtype NumCow = NumCow Int deriving (Eq, Show)

newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

-- A&M: Farmhouse & Farmhouse' are 'the same'.  But the same in what sense?
-- Farmhouse' doesn't have a specialized data constructor, for one.

-- wrapper value for a number of sheep
newtype NumSheep = NumSheep Int deriving (Eq, Show)

-- the regular product type representation of a big farmhouse
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)

-- Alternative representation of BigFarmhouse.
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)


-- Let's define some of the characteristics of some animals
type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo 
  deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)












































-- Constructing values

trivialValue :: GuessWhat
trivialValue = Chickenbutt


idInt :: Id Integer
idInt = MkId 10

-- Type aliases to make the construction function more readable.
type Awesome = Bool
type SomeName = String

-- Create a person
person :: Product SomeName Awesome
person = Product "Simon" True


data Twitter = Twitter deriving (Eq, Show)

data AskFm = AskFm deriving (Eq, Show)

-- Now we use the Sum type.  It mandates that we use the correct data
-- constructor in the correct place.  It can only be "First Twitter" when we list
-- the types in this particular order within the type signature.
socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter


type SN = Sum Twitter AskFm

-- That's equivalent to the following.
data SocialNetwork = Twitter' | AskFm' deriving (Eq, Show)



-- Now how would it look were we to use type aliases (the 'type' keyword) instead
-- of 'data'.
type Twitter'' = String
type AskFm'' = String

-- We have to use a string, because Twitter'' has no data constructor.
twitter :: Sum Twitter'' AskFm''
twitter = First "Twitter"

-- Both are strings.  So it's not type safe.
-- We use First here, but we shouldn't actually be able to use First.
askfm :: Sum Twitter'' AskFm''
askfm = First "AskFm"

-- And now record types.  They use the same syntax as regular product types.
myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.00001

-- There's also another more explicit syntax for record initialization
myRecord' :: RecordProduct Integer Float
myRecord' = RecordProduct { pfirst = 42
                          , psecond = 0.0001 }



-- sum type of OS
data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)


data ProgLang = 
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgLang }
  deriving (Eq, Show)

-- It's more easy to reorder fields when using this specific syntax.
nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

-- We can write the record fields in the constructor in a much different order
-- here.
feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux }




-- We forgot to initialize the 'lang'.  We'll get a warning on load, but no
-- error.  We'll get an exception when we try to evaluate or print the record.
--halfARecord = Programmer { os = GnuPlusLinux }


-- A trinary product type.  Demonstrate partial application
-- Don't use record types that aren't fully evaluated as a 'template'.
data ThereYet = There Float Int Bool deriving (Eq, Show)

nope = There

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yussss :: ThereYet
yussss = notQuite False

-- For explication on deconstructing values, see DeconstructingValues.hs


-- Function type is exponential

data FuturamaCharacter = Fry | Bender | Leela deriving (Eq, Show)

quantSum1 :: Either FuturamaCharacter FuturamaCharacter
quantSum1 = Right Fry

quantSum2 :: Either FuturamaCharacter FuturamaCharacter
quantSum2 = Right Bender

quantSum3 :: Either FuturamaCharacter FuturamaCharacter
quantSum3 = Right Leela

quantSum4 :: Either FuturamaCharacter FuturamaCharacter
quantSum4 = Left Fry

quantSum5 :: Either FuturamaCharacter FuturamaCharacter
quantSum5 = Left Bender

quantSum6 :: Either FuturamaCharacter FuturamaCharacter
quantSum6 = Left Leela

-- 3 * 3 = 9 cardinality on the type of this result.
-- So our enumeration of the possible product types goes all the way up to 10.
quantProd1 :: (FuturamaCharacter, FuturamaCharacter)
quantProd1 = (Fry, Fry)

quantProd2 :: (FuturamaCharacter, FuturamaCharacter)
quantProd2 = (Fry, Bender)

quantProd3 :: (FuturamaCharacter, FuturamaCharacter)
quantProd3 = (Fry, Leela)

quantProd4 :: (FuturamaCharacter, FuturamaCharacter)
quantProd4 = (Bender, Fry)

quantProd5 :: (FuturamaCharacter, FuturamaCharacter)
quantProd5 = (Bender, Bender)

quantProd6 :: (FuturamaCharacter, FuturamaCharacter)
quantProd6 = (Bender, Leela)

quantProd7 :: (FuturamaCharacter, FuturamaCharacter)
quantProd7 = (Leela, Fry)

quantProd8 :: (FuturamaCharacter, FuturamaCharacter)
quantProd8 = (Leela, Bender)

quantProd9 :: (FuturamaCharacter, FuturamaCharacter)
quantProd9 = (Leela, Leela)



-- Now we extend that to the function type.

-- We handle all cases.

-- We start off by iterating on the final case, all values of the 3-valued sum
-- type.
quantFlip1 :: FuturamaCharacter -> FuturamaCharacter
quantFlip1 Fry = Fry
quantFlip1 Bender = Fry
quantFlip1 Leela = Fry

quantFlip2 :: FuturamaCharacter -> FuturamaCharacter
quantFlip2 Fry = Fry
quantFlip2 Bender = Fry
quantFlip2 Leela = Bender

quantFlip3 :: FuturamaCharacter -> FuturamaCharacter
quantFlip3 Fry = Fry
quantFlip3 Bender = Fry
quantFlip3 Leela = Leela

-- On the fourth case, we start iterating the second-last value on all remaining
-- values that it has.
quantFlip4 :: FuturamaCharacter -> FuturamaCharacter
quantFlip4 Fry = Fry
quantFlip4 Bender = Bender
quantFlip4 Leela = Fry

quantFlip5 :: FuturamaCharacter -> FuturamaCharacter
quantFlip5 Fry = Fry
quantFlip5 Bender = Leela
quantFlip5 Leela = Fry

-- ... There should be 27 possibilities in total.

-- Exponentiation in what order?

-- As the type algebra operates in the order (b^a), there should be
-- 2^3 = 8 implementations of this function.
-- These are all of them written out...
-- Confirmed in sort | uniq that this is all correct implementations.
-- So exponential means -- given the lack of any information about the argument
-- beside its type.  We know that the expression `myFn x` has N possible values
-- as long as it type checks.
convert1 :: FuturamaCharacter -> Bool
convert1 Fry = False
convert1 Bender = False
convert1 Leela = False

convert2 :: FuturamaCharacter -> Bool
convert2 Fry = False
convert2 Bender = False
convert2 Leela = True

convert3 :: FuturamaCharacter -> Bool
convert3 Fry = False
convert3 Bender = True
convert3 Leela = False

convert4 :: FuturamaCharacter -> Bool
convert4 Fry = True
convert4 Bender = False
convert4 Leela = False

convert5 :: FuturamaCharacter -> Bool
convert5 Fry = False
convert5 Bender = True
convert5 Leela = True

convert6 :: FuturamaCharacter -> Bool
convert6 Fry = True
convert6 Bender = False
convert6 Leela = True

convert7 :: FuturamaCharacter -> Bool
convert7 Fry = True
convert7 Bender = True
convert7 Leela = False

convert8 :: FuturamaCharacter -> Bool
convert8 Fry = True
convert8 Bender = True
convert8 Leela = True

