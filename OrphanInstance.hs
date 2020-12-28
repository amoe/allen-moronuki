module OrphanInstance where

import Listy

-- Orphan instance, we don't define either the type class or the type itself in
-- our module.

-- Listy.hs:10:10-28: error: …
--     Duplicate instance declarations:
--       instance [safe] Semigroup (Listy a) -- Defined at Listy.hs:10:10
--       instance Semigroup (Listy a)
--         -- Defined at /home/amoe/dev/allen-moronuki/OrphanInstance.hs:7:10
--    |
-- Compilation failed.
  
instance Semigroup (Listy a) where
  (<>) (Listy l) (Listy r) = Listy $ mappend l r

instance Monoid (Listy a) where
  mempty = Listy []
