-- import Monad
import Exception

data Term = Con Int | Div Term Term

eval :: Monad m => Term -> m Int
eval (Con x) = return x
eval (Div t1 t2) = do x <- eval t1
                      y <- eval t2
                      return (div x y)
