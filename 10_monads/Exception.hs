module Exception where

data Exc a = Raise Exception | Return a
  deriving (Show)
type Exception = String
