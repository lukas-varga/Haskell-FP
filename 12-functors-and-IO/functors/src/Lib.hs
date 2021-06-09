module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc =  putStr $ fmap' (+1) 5


-------------------- EXERCISE 01 --------------------
fmap' :: ( a -> b ) -> IO a -> IO b
fmap' f m
    = do    x <- m
            return (f x)

-------------------- EXERCISE 02 --------------------
repeat :: IO Bool -> IO () -> IO ()
repeat = undefined


