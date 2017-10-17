-- Code for the third chapter: Numbers

import Prelude hiding ((+))

data Nat = Zero | Succ Nat

instance Show Nat where
  show Zero = show 0
  show (Succ n) = "Succ("++ show n ++ ")"

instance Eq Nat where
  (==) Zero Zero = True
  (==) (Succ n) Zero = False
  (==) Zero (Succ n) = False
  (==) (Succ m) (Succ n) =  m == n

instance Ord Nat where
    (<=) Zero Zero = True
    (<=) (Succ n) Zero = False
    (<=) Zero (Succ n) = True
    (<=) (Succ m) (Succ n) =  m <= n


bottom :: a
bottom = bottom
----
-- Observar que bottom != Succ(bottom)
-- zero != Succ(bottom)
--

infty :: Nat
infty = Succ(infty)

---
-- fold functions
--

foldn :: (a -> a) -> a -> Nat -> a
foldn h c Zero = c
foldn h c (Succ n) = h (foldn h c n)
--
-- Defino + usando foldn!
--
-- (+) :: Nat -> Nat -> Nat
-- Zero + n = n
-- (Succ m) + n = Succ(m Main.+ n)

(+) :: Nat -> Nat -> Nat
(+) m n = foldn Succ m n

(*) :: Nat -> Nat -> Nat
(*) m n = foldn ((+) m) m n
