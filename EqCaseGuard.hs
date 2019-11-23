module EqCaseGuard where

data PersonInvalid = NameEmpty | AgeTooLow

-- Note that we don't need Eq for this to compile
toString :: PersonInvalid -> String
toString NameEmpty = "The person's name was empty."
toString AgeTooLow = "The person's age was too low."

instance Show PersonInvalid where
  show = toString

-- However this won't work because of the use of (==)
-- But it does work after we defined the Eq type class on the data type, below.
blah :: PersonInvalid -> String
blah pi 
  | pi == NameEmpty = "The person's name was empty."
  | pi == AgeTooLow = "The person's age was too low."
  | otherwise = "???"

instance Eq PersonInvalid where
  (==) NameEmpty NameEmpty = True
  (==) AgeTooLow AgeTooLow = True
  (==) _ _ = False
