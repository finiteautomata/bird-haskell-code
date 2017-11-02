module Term where
data Term = Con Int | Div Term Term
  deriving (Show)
