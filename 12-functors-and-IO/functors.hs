-------------------- EXERCISE 01 --------------------
fmap' :: ( a -> b ) -> IO a -> IO b
fmap' f m
    = do    x <- m
            return (f x)

-------------------- EXERCISE 02 --------------------
repeat' :: IO Bool -> IO () -> IO ()
repeat' test m
    = do    res <- test
            if res then return ()
            else do m
                    repeat' test m
            
-------------------- EXERCISE 03 --------------------
whileG :: (a -> IO Bool) -> (a -> IO a) -> (a -> IO a)
whileG cond op x
    = do    res <- cond x
            if res then
                do  op x
                    whileG cond op x
            else return x