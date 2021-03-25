---------- Exercise 1 ----------
a1 = ['a','b','c'] --[Char]
a2 = ('a', 'b', 'c') --(Char,Char,Char)
a3 = [(False, '0'), (True, '1')] --[(Bool,Char)]
a4 = (['1', '0'], ['0', '1']) --([Char],[Char])
a5 = [tail, init, reverse] --[Function]


---------- Exercise 2 ----------
bools :: [Bool]
bools =  [True, True]

nums :: [[Int]]
nums = [[2,3,5,1],[2,3,5,1]]

add :: Int -> Int -> Int-> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy a = (a,a) 

apply :: (a -> b) -> a -> b
apply f x = f(x) 

---------- Exercise 3 ----------
second :: Ord a => [a] -> a
second xs = head (tail xs)

swap :: (Eq a, Eq b) => (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: (Eq a, Eq b) => a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

pallindrome :: Ord a => [a] -> Bool
pallindrome xs = reverse xs == xs

twice :: Eq a => (a -> a) -> a -> a
twice f x = f (f x)