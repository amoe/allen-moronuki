module Listy where

-- Used with OrphanInstance.hs (p594 example).

newtype Listy a = Listy [a] deriving (Eq, Show)

-- Identical to a definition that would use (++) directly, because we're
-- already asserted that l and r are actual lists.  We are not asserting that
-- the actual `a` value has any semigroup definition.
instance Semigroup (Listy a) where
  (<>) (Listy l) (Listy r) = Listy $ mappend l r

  
  
