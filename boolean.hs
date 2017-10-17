--
-- Datatypes: Mi propio booleano
--
data Booleano = Verdadero | Falso

no :: Booleano -> Booleano
no Verdadero = Falso
no Falso = Verdadero

-- Notar que no es estricto en el segundo parámetro
(^) :: Booleano -> Booleano -> Booleano
Verdadero ^ x = x
Falso ^ x = Falso


-- Notar que no es estricto en el segundo parámetro
(||) :: Booleano -> Booleano -> Booleano
Verdadero || x = Verdadero
Falso || x = x

class Igualdad a where
  (==) :: a -> a -> Booleano

instance Igualdad Booleano where
  x == y = (x Main.^y) Main.|| (no x Main.^ no y)
