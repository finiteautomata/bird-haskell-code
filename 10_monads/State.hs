-- As another variation, suppose we want to count the number of divisions
-- performed during evaluation. One way is to introduce an additional component,
-- called the state. The state in this instance is an integer, initialised to zero at the
-- start of the computation, and incremented by one each time a division occurs.10.2 / Variations on an evaluator
-- Such a state may be mimicked by introducing a type to represent computations
-- that act on state, called a state transformer:

-- It is not quite clear what a "state" is
-- Well, this is strange: but "state" is the count of divisions
import Term

newtype St a = MkSt (State -> (a, State))
type State = Int

apply :: St a -> State -> (a, State)
apply (MkSt f) s = f s

instance Show a => Show (St a) where
  show st = "value: " ++ show x ++ ", count: " ++ show s
    where (x, s) = apply st 0

eval :: Term -> St Int
eval (Con x) = MkSt f
  where f s = (x, s)
eval (Div t1 t2) = MkSt f
  where f s = (div x y, s2 + 1)
          where (x, s1) = apply (eval t1) s
                -- Observe that we pass the state returned by `apply (eval t1) s`
                (y, s2) = apply (eval t2) s1
