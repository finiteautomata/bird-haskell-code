module Exception where
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

data Exc a = Raise Exception | Return a
  deriving (Show)
type Exception = String
--  This is raising an exception!
-- We have to define fmap for this...

-- As of ghci 7.0 (I think) we have to declare a Monad as Applicative
instance Functor Exc where
  fmap = liftM

instance Applicative Exc where
  pure  = return
  (<*>) = ap


instance Monad Exc where
  return x = Return x
  (Raise e) >>= q = Raise e
  (Return x) >>= q = q x


raise :: Exception -> Exc a
raise e = Raise e
