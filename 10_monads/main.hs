import Data.Typeable

main = do putStrLn "Insert a number:"
          x <- getLine
          -- I don't quite understand why line below doesn't work without let
          -- I mean: z <- (read x :: Integer)
          -- Is it perhaps it doesn't return a Monad?
          -- Yup! See https://stackoverflow.com/questions/9932913/bindings-in-do-notation

          let z = (read x :: Integer)
          let msg = "Successor of " ++ show z ++ " is " ++ show (z+1)
          putStrLn $ "Type of z : " ++ (show (typeOf z))
          putStrLn $ "Type of msg : "  ++(show (typeOf msg))
          putStrLn msg
          --putStrLn
          return ()
