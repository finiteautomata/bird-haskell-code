import Term

newtype Out a = MkOut (Output, a)
  deriving (Show)
type Output = String

line :: Term ->  Int -> Output
line t x = "term: " ++ show t ++ ", yields " ++ show x ++ "\n"

eval :: Term -> Out Int
eval (Con x) = MkOut (line (Con x) x, x)
eval (Div t u) = MkOut (o1 ++ o2 ++ line (Div t u) z, z)
  where MkOut (o1, q) = eval t
        MkOut (o2, d) = eval u
        z = div q d
