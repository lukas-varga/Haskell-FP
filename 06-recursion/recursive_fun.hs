--------------------EXERCIS 01--------------------
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

--------------------EXERCIS 02--------------------
exponention :: Int -> Int -> Int
exponention 0 _ = 0
exponention _ 0 = 1
exponention x e = x * exponention x (e-1)

--------------------EXERCIS 03--------------------
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

--------------------EXERCIS 04--------------------
myInit :: [a] -> [a]
myInit [] = []
myInit [x] = []
myInit (x:xs) = [x] ++ myInit xs

--------------------EXERCIS 05--------------------
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs)
    | x == False = False
    | otherwise = myAnd xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat [[]] = []
myConcat (x:xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = [x] ++ myReplicate (n-1) x

myNth :: [a] -> Int -> a 
myNth [] _ = error "empty list"
myNth (x:xs) 1 = x
--myNth (x:xs) 0 = x
myNth (x:xs) i = myNth xs (i-1)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
        | x == e = True
        | otherwise = myElem e xs

--------------------EXERCIS 06--------------------
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs 

take' :: Int -> [a] -> [a]
take' 0 (x:xs) = []
take' _ [] = []
take' n (x:xs) = [x] ++ take' (n-1) xs

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' (xs)
--------------------EXERCIS 07--------------------
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) | x <= y    = [x] ++ merge xs (y:ys)
                    | otherwise = [y] ++ merge (x:xs) ys

--------------------EXERCIS 08--------------------
halve :: [a] -> ([a], [a])
halve [x] = ([x],[])
halve xs = (takeFrst, takeScnd) where
    takeFrst = take half xs
    takeScnd = drop half xs
    half = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right) where
    (left, right) = halve xs
