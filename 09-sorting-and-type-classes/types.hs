--------------------EXERCISE-01--------------------
safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

safetail' :: [a] -> Maybe [a]
safetail' [] = Nothing
safetail' (_:xs) = Just xs

safetail'' :: [a] -> Either String [a]
safetail'' [] = Left "Empty list!"
safetail'' (_:xs) = Right xs

--------------------EXERCISE-02--------------------
{-
1.  Val 1
    value (Val 1)
    1

2.  Add ( Val 1 ) ( Val 2 )
    value (Val 1) + value (Val 2)
    1 + 2
    3

3.  (Add (Add ( Val 2 ) ( Val 3 ) ) ( Val 4 ) )
    value (Add (Val 2) (Val 3)) + value (Val 4)
    value (value (Val 2) + value (val 3)) + 4
    value (2+3)+4
    value (5)+4
    5+4
    9
-}

--------------------EXERCISE-03--------------------
data Expr = Val Int | Add Expr Expr | Mult Expr Expr
data Op = EVAL Expr | ADD Int | MULT Int
type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)
eval (Mult x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MULT n : c) m = exec c (n * m)

val :: Expr -> Int
val e = eval e []

--------------------EXERCISE-04--------------------
{-
mult :: Nat -> Nat -> Nat
mult Zero n     = n
mult (Succ m) n = Succ (mult m n)
-}

--------------------EXERCISE-05-------------------
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = compare x y == EQ
occurs' x (Node l y r)  | compare x y == EQ = True
                        | compare x y == LT = occurs' x l
                        | compare x y == GT = occurs' x r

--------------------EXERCISE-06-------------------
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
    deriving (Show)

t' :: Tree' Int
t' = Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3) (Leaf' 4))

t'' :: Tree' Int
t'' = Node' (Node' (Node' (Leaf' 1)(Leaf' 2)) (Leaf' 3)) (Leaf' 4)

{-
flatten :: Tree' a -> [a]
flatten (Leaf' x) = [x]
flatten (Node' l r) = flatten l ++ flatten r

leaves :: Tree' a -> Int
leaves a = length $ flatten a
-}

leaves :: Tree' a -> Int
leaves (Leaf' x)    = 1
leaves (Node' l r)  = leaves l + leaves r + 1 

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = (diff <= 1) && balanced l && balanced r
    where   diff = abs (leaves l - leaves r)

depth :: Tree' a -> Int
depth (Leaf' _) = 0
depth (Node' l r) = max (depth l + 1) (depth r + 1)

--------------------EXERCISE-07-------------------
balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance frst) (balance scnd)
    where   frst = fst $ halve xs
            scnd = snd $ halve xs

halve :: [a] -> ([a], [a]) 
halve xs = 
    ((take s xs), (drop s xs))
    where
        s = (length xs ) `div` 2

--------------------EXERCISE-08-------------------
data Tree''' a = EmptyTree 
            | Node''' (Tree''' a) a (Tree''' a)
                deriving (Show, Read, Eq)

t''' :: Tree''' Int
t''' = Node''' (Node''' (EmptyTree) 2 (EmptyTree)) 1 (Node''' (Node''' (EmptyTree) 4 (EmptyTree)) 3 (EmptyTree))

occurs''' :: Ord a => a -> Tree''' a -> Bool
occurs''' _ (EmptyTree) = False
occurs''' x (Node''' l y r)
    | x == y    = True
    | x < y     = occurs''' x l
    | x > y     = occurs''' x r

treeInsert''' :: (Ord a) => a -> Tree''' a -> Tree''' a
treeInsert''' x (EmptyTree)     = Node''' (EmptyTree) x (EmptyTree)
treeInsert''' x (Node''' l y r) = Node''' (treeInsert''' x l) y r

flatten''' :: Tree''' a -> [a]
flatten''' (EmptyTree) = []
flatten''' (Node''' l x r) = flatten''' l ++ [x] ++ flatten''' r

--------------------EXERCISE-09-------------------
data Tree'''' a = Leaf'''' | Node'''' a ( Tree'''' a ) ( Tree'''' a )
        deriving Show

myTree :: Tree'''' Int
myTree = Node'''' 1 ( Node'''' 6 ( Node'''' 4 ( Leaf'''' ) ( Leaf'''' ) ) ( Leaf'''' ) )( Node'''' 3 Leaf'''' Leaf'''' )

repeat' :: Tree'''' a -> [ Tree'''' a ]
repeat' x = xs where xs = x : xs

take' :: Int -> Tree'''' a -> Tree'''' a 
take' 0 _        = Leaf''''
take' _ Leaf'''' = Leaf''''
take' n (Node'''' x l r) = Node'''' x (take' (n-1) l) (take' (n-1) r)

replicate' :: Int -> Tree'''' a -> [Tree'''' a]
replicate' n = take n . repeat

map' :: (a -> b) -> Tree'''' a -> Tree'''' b
map' _ Leaf'''' = Leaf''''
map' f (Node'''' x l r) = Node'''' (f x) (map' f l) (map' f r)
