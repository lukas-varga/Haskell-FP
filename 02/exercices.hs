a = (2^3)*4
b = (2 * 3) + (4 * 5)
c = 2 + (3 * (4^5))

n = let a = 10
        xs = [1,2,3,4,5]
        in a `div` length xs

m = let xs = [1,2,3,4,5]
        in head (reverse xs)

o = let xs = [1,2,3,4,5]
        in reverse (drop 1 (reverse xs))
