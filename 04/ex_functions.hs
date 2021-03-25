--FUNCTIONS
---------- Exercise 1 ----------
add1 :: Int -> Int
add1 a = a + 1 

---------- Exercise 2 ----------
always0 :: Int -> Int
always0 _ = 0

---------- Exercise 3 ----------
subtract' :: Int -> Int-> Int
subtract' a b = a - b

---------- Exercise 4 ----------
addmult :: Int-> Int -> Int-> Int
addmult p q r = (p + q) * r


--CONDITIONALS
---------- Exercise 5 ----------
greaterThan0 :: Int -> String
greaterThan0 n = if n > 0 then "Yes" else "No"

---------- Exercise 6 ----------
pushOut :: Int -> Int
pushOut n = if n>0 then n+1
            else if n<0 then n-1
            else 0

pushOut' :: Int -> Int
pushOut' n  | n>0       = n+1
            | n<0       = n-1
            | otherwise = 0

---------- Exercise 7 ----------
halve :: [a] -> ([a], [a])
halve   xs =  if length xs `mod` 2 == 0 then (take half xs, drop half xs)
            else (xs,[]) where 
        half = length xs `div` 2
{-
halve   xs = (take half xs, drop half xs) where
        half = length xs `div` 2
-}

---------- Exercise 8 ----------
third :: [a] -> a
third xs =  if length xs >= 3 then head(tail(tail xs))
            else head xs

third' :: [a] -> a
third' xs =  if length xs >= 3 then xs !! 2
            else head xs

third'' :: [a] -> a
third'' (_:_:x:_) = x

