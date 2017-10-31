import Exception


data Term = Con Int | Div Term Term | Try Term Term

try :: Exc a -> Exc a -> Exc a
try (Return x) _ = Return x
try (Raise e) (Return y) = (Return y)
try (Raise e1) (Raise e2) = (Raise e2)


-- 10.2.1 Modify the evaluator with exceptions to specify an alternative term to
-- evaluate in case of an exception. More specifically, extend the type Term to
-- include a new term Try Term Term. To evaluate Try t u, first evaluate t and, if it
-- succeeds, return its value; but if evaluation raises an exception, then evaluate
-- u. Here's a transcript of how the program should behave:
-- ? eval (Try (Div (Con 1) (Con 0)) (Con 42))
-- 42
-- Define a corresponding try operation on the type Exc to facilitate the
-- modification.

eval :: Term -> Exc Int
eval (Con x) = Return x
eval (Div t1 t2) = do x <- eval t1
                      y <- eval t2
                      if y == 0
                        then raise "Division by zero"
                        else return (x `div` y)
eval (Try t1 t2) = try (eval t1) (eval t2)
