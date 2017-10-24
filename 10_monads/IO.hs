-- Ejemplo de Mónadas (Capítulo 10)

-- Básicamente, las mónadas son objetos M que tienen las siguientes operaciones
-- 1) return :: a -> M a
-- 2) >>= M a -> (a -> M b) -> M b
--
--
-- El comando m1 >> m2 ejecuta primero m1, luego m2 y retorna lo que esta devuelve

write :: String -> IO()
write [] = return ()
write (x:xs) = putChar x >> write xs

-- El comando m1>>=f ejecuta m1, y luego llama f con lo que devolvió m1
readn :: Int -> IO String
readn 0 = return []
readn n = getChar >>= q
  where q c = readn (n-1) >>= r
          where r cs = return (c:cs)

-- do
-- Para evitar usar la notación >= (súper engorrosa), usaremos do
-- Las reglas de do son
--
-- do {r} -> r
-- do {x <- p, C, r} -> p >= q
--    where q x = do {C, r}
--
-- Esto nos permite "asignar" variables
--

readln :: IO String
readln = do c <- getChar
            if c == '\n' then
               return []
            else do cs <- readln
                    return (c:cs)

palindrome :: IO ()
palindrome = do l <- getLine
                if reverse l == l
                  then write "YES"
                  else write "NO"
