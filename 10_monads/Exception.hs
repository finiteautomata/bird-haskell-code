module Exception where

data Exc a = Raise Exception | Return a
  deriving (Show)
type Exception = String
--  This is raising an exception!
-- We have to define fmap for this...
-- instance Monad Exc where
--   return x = Return x
--   (Raise e) >>= q = Raise e
--   (Return x) >>= q = q x

raise :: Exception -> Exc a
raise e = Raise e
