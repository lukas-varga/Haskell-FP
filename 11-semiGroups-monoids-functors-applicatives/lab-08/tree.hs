data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

instance Functor Tree where
      fmap _ Leaf = Leaf
      fmap f (Node left value right) = Node (fmap f left) (f value) (fmap f right)

instance Applicative Tree where
        pure x = let t = Node t x t
           in t

        (<*>) Leaf _ = Leaf
        (<*>) _ Leaf = Leaf
        (<*>) (Node lf f rf) (Node la a ra) = Node (lf <*> la) (f a) (rf <*> ra)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf     =  Node Leaf x Leaf
insert x (Node  left top right)
          |x == top = Node left top right
          |x < top  = Node (insert x left) top right
          |x > top  = Node left top (insert x right)

flatten :: Tree a -> [a]
flatten Leaf = []
flatten (Node left top right) =
         flatten left ++ [top] ++ flatten right

initTree :: (Ord a )=>  [a]-> Tree a
inittree  [] = Leaf
initTree [x] = Node Leaf x Leaf
initTree (x:xs) = insert x (initTree xs)

double :: Num a => Tree a -> Tree a
double t = fmap (\x -> x*2) t

doubleAll :: Num a => [Tree a] -> [Tree a]
doubleAll xs = fmap double xs

-- ghci > [double] <*> [initTree [1..3],initTree [2..5]]