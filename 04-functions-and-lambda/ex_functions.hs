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
third' xs =     if length xs >= 3 then xs !! 2
                else head xs

third'' :: [a] -> a
third'' (_:_:x:_) = x

---------- Exercise 9 ----------
safetail :: [a] -> [a]
safetail xs =   if isFull then tail xs
                else xs 
        where
        isFull = not (null xs)
                
safetail' :: [a] -> [a]
safetail' xs    | isFull = tail xs
                | otherwise = xs               
        where
        isFull = not (null xs)

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs 

---------- Exercise 10 ----------
myOr :: Bool -> Bool -> Bool
True `myOr` True = True
True `myOr` False = True
False `myOr` True = True
False `myOr` False = False

myOr' :: Bool -> Bool -> Bool
False `myOr'` False = False
_ `myOr'` _ = True

myOr'' :: Bool -> Bool -> Bool
False `myOr''` b = b
True `myOr''` _ = True

---------- Exercise 11 ----------
lucky :: Integral a => a-> String
lucky x | x == 7 = "Lucky you.. Proceed directly to buy a lottery ticket."
        | x == 13 = "You, sadly are quite unlucky. Do not, under any circumstances, invest money today"
        | otherwise = "Mmmm.... Can't really say...."

---------- Exercise 12 ----------
myFst :: (a,b,c) -> a
myFst (x,_,_) = x
mySnd :: (a,b,c) -> b
mySnd (_,y,_) = y
myThd :: (a,b,c) -> c
myThd (_,_,z) = z

---------- Exercise 13 ----------
luhnDouble :: Int -> Int
luhnDouble x
        | doubleX > 9 = doubleX - 9
        | otherwise = doubleX
        where
        doubleX = x*2

luhn :: Int -> Int-> Int-> Int-> Bool
luhn a b c d
        | sum [luhnDouble a, b, luhnDouble c, d] `mod` 10 == 0 = True
        | otherwise = False

---------- Exercise 14 ----------
luhnGetCheck:: Int -> Int-> Int-> Int
luhnGetCheck a b c = calc
        where 
        calc = 10 - (sum [luhnDouble a, b, luhnDouble c] `mod` 10) 

