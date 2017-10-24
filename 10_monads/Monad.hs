-- Definición de la clase mónada
-- Observemos que m no puede ser cualquier cosa: tiene que construir un tipo a través de otro parámetro
module Monad where 
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
