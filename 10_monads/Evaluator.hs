-- import Monad
module Evaluator where
import Exception

data Term = Con Int | Div Term Term

eval :: Monad m => Term -> m Int
eval (Con x) = return x
eval (Div t1 t2) = do x <- eval t1
                      y <- eval t2
                      return (div x y)

evalEx :: Term -> Exc Int
evalEx (Con x) = Return x
evalEx (Div t1 t2) = do x <- evalEx t1
                        y <- evalEx t2
                        if y == 0
                          then raise "Division by zero"
                          else return (x `div` y)
