import Debug.Trace

multby2 :: Num a=> [a] -> [a]
multby2 [] = []
multby2 (x:xs) = x*2 : multby2 xs

multby2' :: Num a=> [a] -> [a]
multby2' [] = trace "running on empty" []
multby2' (x:xs) = trace "recursing"  x*2 : multby2' xs 

--Note use of (Num a, Show a) here - we need to use show. 
multby2'' :: (Num a, Show a) => [a] -> [a]
multby2'' [] = trace "running on empty" []
multby2'' (x:xs) = trace ("value of x = "  ++ show x)  x*2 : multby2'' xs

----------EXERCISE-01-----------
fact :: (Integral a, Show a) => a -> a
fact 0 = trace "end of recursion" 1
fact x = trace("value of x = " ++ show x) x * fact (x-1) 

----------EXERCISE-02-----------
sort :: (Show a, Ord a) => [a] -> [a]
sort [] = []
sort ( x : xs ) = trace ("sort x = " ++ show x) insert x ( sort xs )

insert :: (Show a, Ord a) => a -> [a] -> [a]
insert x [ ] = trace ("empty x = " ++ show x) [ x ]
insert x ( y : ys ) =   if trace ("if x = "++ show x ++ "; y = "++show y)  x <= y then (x : y : ys)  
                        else trace ("else x = "++ show x ++ "; y = "++show y) y : insert x  ys