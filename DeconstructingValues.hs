module DeconstructingValues where

ch11 = 42

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

-- Sum type for a farmer.
data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show

-- Determine through pattern matching whether argument is a DairyFarmer
isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False


-- A record type demonstrating the same thing.
data FarmerRec = FarmerRec { name :: Name
                           , acres :: Acres
                           , farmerType :: FarmerType }
                 deriving Show

-- Deconstruct the value by explicitly using the accessor function.
isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = 
  case farmerType farmer of
    DairyFarmer -> True
    _ -> False
