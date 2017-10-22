-- Code for chapter 04 - Lists
module List (List(Nil, Cons)) where
data List a = Nil | Cons a (List a)

instance Show a => Show(List a) where
  show Nil = "[]"
  show (Cons x ls) = (show x) ++ ":" ++ (show ls)
