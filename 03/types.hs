----------------------------------
-- add a type declaration
-- to each of the named expressions
i1:: Integer  -- I have done the first one for you
i1 = 45 

i2 = "123" :: String

i3 = 45 <= i1 :: Bool

i4 = 'c' :: Char

i5 = ["abc","ok"] :: [String]

i6 = head i5 :: String

i7 = tail "abc" :: [Char]-- Recall a string is a shorthand for a list of Char

i8 = (True,4.5) :: (Bool,Double)

i9 = [i1,34] :: [Integer]

-------------------------------------------------
-- For each named expression replace "undefined"
-- with an expression with the same type as the declaration

j1:: (String,Integer)
j1 = ("Lukas",42)

j2:: [Integer]
j2 = [0..50]

j3:: Char
j3 = '@'

j4:: Double
j4 = 3.14

j5:: (Integer,String,Integer,Char)
j5 = (5,[j3,j3],5,j3)

j6:: ([Char],(Bool,String))
j6 = (['c','a'],(True,"Lukas"))

j7:: [[Bool]]
j7 = [[True,False,True],[False,False,True],[True,True,True]]

j8:: [(String,Bool)]
j8 = [("Lukas",False),("Jan",True)]