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

-------------------- EXERCISE 04 --------------------
getInteger :: String -> IO Integer
getInteger prompt
    = do    putStr prompt
            st <- getLine
            return (read st :: Integer)

findAvg :: IO Integer
findAvg
    = do    n <- getInteger "N: "
            s <- sumInts n 0
            return (s `div` n)


sumInts :: Integer -> Integer -> IO Integer
sumInts n s
    = if n>0
        then do m <- getInteger "Number: "
                sumInts (n-1) (s+m)
        else return s

-------------------- EXERCISE 05 --------------------
accumulate :: [IO a] -> IO [a]
accumulate [] = return []
accumulate (a:as)
    = do    x <- a
            xs <- accumulate as
            return (x:xs)
    
sequence' :: [IO a] -> IO ()
sequence' [] = return ()
sequence' (a:as)
    = do    a
            sequence' as
            return ()

seqList :: [a -> IO a] -> a -> IO a
seqList [] elem = return elem
seqList (a:as) elem
    = do    x <- a elem
            seqList as x

testf :: String -> IO String
testf x = do
    putStrLn x
    return ( x ++ x )

-------------------- EXERCISE 06 --------------------
data Result a = Succeed a | Fail
    deriving (Eq, Show)

instance Functor Result where
    fmap f (Succeed x)  = Succeed (f x)
    fmap _ _            = Fail

instance Applicative Result where
    pure = Succeed
    Fail <*> _ = Fail
    (Succeed f) <*> something = fmap f something

instance Monad Result where
    return = Succeed
    Succeed x >>= f = f x
    Fail >>= _      = Fail

divBy :: Int -> Int -> Result Int
divBy 0 _   = Fail
divBy x y   = Succeed (y `div` x)