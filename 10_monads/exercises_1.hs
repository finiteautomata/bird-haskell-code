--- 10.1.1 Define >> in terms of >>=
-- I define it with other name (|||)
(|||) :: Monad m => m a -> m b -> m b
(|||) x y = x >>= q
  where q l = y

--- 10.1.2 What is the effect of the command foldl (») done . map putChar?
-- It is just another write!
-- Remember here done is return ()
myWrite :: String -> IO ()
myWrite = (foldl (>>) (return ())) . map putChar

--- 10.1.3 When performed, getChar reads a character and returns the character.
--- Describe the effect of performing getChar >>= return. What relationship between >>=
---  and return does this suggest?
-- Let's look at the type of it
--    *Main> :t getChar >>= return
--    getChar >>= return :: IO Char
-- So, return in this case does nothing. Does this means that return is "neutral"
-- element of >>= operator?


--- 10.1.4 Describe the effect of performing return '!' >>= putChar. What
--- relationship between >>= and return does this suggest?
-- In this case, this is the same as doing putChar '!'
-- So, return a >>= f is the same as f a

--- Using >>=, define an operator o with type
-- (o) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
(||||) :: (a -> IO b) -> (b -> IO c) -> a -> IO c
(||||) f g a = (f a) >>= g
