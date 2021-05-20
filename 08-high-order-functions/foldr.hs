--------------------EXERCISE 01--------------------
sumsq :: Integral a => a -> a
sumsq n = foldr fun 0 [1..n] where
    fun :: Num a => a -> a -> a
    fun x y = x*x + y
--------------------EXERCISE 02--------------------
lengthr :: [Int] -> Int
lengthr (x:xs) = foldr (\x y -> y+1) 0 (x:xs)

--------------------EXERCISE 03--------------------
minlistr :: [Int] -> Int
minlistr = foldr1 min

--------------------EXERCISE 04--------------------
myreverse :: [a] -> [a]
myreverse xs = foldr (\x y -> y ++ [x]) [] xs

--------------------EXERCISE 05--------------------
myremove :: Eq a => [a] -> [a] -> [a]
myremove xs = foldr (\y accum -> (helper y xs) ++ accum) []
    where helper x ys = if x `elem` ys then []
                        else [x]


--------------------EXERCISE 06--------------------
remdupsr :: Eq a => [a] -> [a]
remdupsr [] = []
remdupsr xs = foldr joinr [] xs 

joinr :: Eq a => a -> [a] -> [a]
joinr x []      = [x]
joinr x xs
    | x == head xs  = xs
    | otherwise     = [x] ++ xs

