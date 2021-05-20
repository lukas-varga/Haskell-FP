-------------------- EXERCISE 1 --------------------
--1. b
--2. c
--3. b

-------------------- EXERCISE 2 ----------------------
{- 
all of them are equal

mth x y z = x * y * z

mth x y = \z -> x * y * z

mth x = \y -> \z -> x * y * z

mth = \x -> \y -> \z -> x * y * z
-}


-------------------- EXERCISE 3 --------------------
-- it has the same type as mth x y z
mth :: Num a=> a-> a-> a-> a
mth x = \y -> \z -> x * y * z

-------------------- EXERCISE 4 --------------------
--Done using prompt
-- <>(\x -> x)2                 => 2
-- <>(\x -> (x*2))4             => 8
-- <>(\x -> (\y -> x*y))3 4     => 12
-- <>(\x -> \y -> (if x < y then (-1) else if x == y then 0 else 1))3 4
--                              =>-1

-------------------- EXERCISE 5 --------------------
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f n = n + 1

myAddOneIfOdd = \n -> case odd n of
    True -> (n+1)
    False -> n

-------------------- EXERCISE 6 --------------------
addFive x y = (if x > y then x else y) + 5
myAddFive = (\x -> \y -> if x > y then x else y + 5)

-------------------- EXERCISE 7 --------------------
--myAbs :: Num a => a -> a
myAbs = \x -> if x >= 0 then x else (-x)

mymax :: Ord a => a -> a -> a
mymax = (\x -> \y -> if x > y then x else y)

mymin :: Ord a => a -> a -> a
mymin = (\x -> \y -> if x < y then x else y)

-------------------- EXERCISE 7 --------------------
{-
NOT =   (\x -> x FALSE TRUE)
AND =   (\x -> \y -> x y FALSE)
OR  =   (\x -> \y -> x TRUE y)
-}