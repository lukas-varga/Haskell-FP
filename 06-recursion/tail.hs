--------------------EXERCISE 01--------------------
fibT :: Int -> Int
fibT n = helper n 1 0 where
    helper 1 prev prevBut1 = prev
    helper n prev prevBut1 = helper (n-1) (prev+prevBut1) prev

--------------------EXERCISE 02--------------------
myAdd :: Int -> Int -> Int
myAdd x 0 = x
myAdd 0 y = y
myAdd x y = myAdd (x-1) (y+1)

myMult :: Int -> Int -> Int
myMult x 1 = x
myMult 1 y = y
myMult x y =  x + myMult x (y-1)

tailMult :: Int -> Int -> Int
tailMult x y = helperMult x y 0 where
    helperMult x 0 acc = acc 
    helperMult x y acc = helperMult x (y-1) (acc+x)

--------------------EXERCISE 03--------------------    
reverse_ :: [a] -> [a]
reverse_ [] = []
reverse_ (x:xs) = reverse_ xs ++ [x]

reverseAccum :: [a] -> [a]
reverseAccum list = revHelper [] list where
    revHelper accum []      = accum
    revHelper accum (x:xs)  = revHelper (x:accum) xs