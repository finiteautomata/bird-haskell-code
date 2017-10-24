import Monad
import Exception

data Term = Con Int | Div Term Term

eval :: Term -> Exc Int
eval (Con x) = Return x
eval (Div t1 t2) = h (eval t1)
  where h (Raise e) = Raise e
        h (Return x) = h' (eval t2)
          where
            h'(Raise e) = Raise e
            h'(Return y) = if y == 0
                             then Raise "division by zero"
                             else Return (div x y)
