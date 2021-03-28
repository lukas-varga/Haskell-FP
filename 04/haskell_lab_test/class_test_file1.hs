-- Lab test for Haskell. This is worth 5% of your overall score. This test is worth 100 Marks. 
-- This file is made up of 
-- 		SECTION A - 30 Mark
--	 	SECTION B - 30 Marks
--		SECTION C - 40 Marks  (this is in clas_test_fixing errors.hs) )

-- Please put your name below (where indicated) and zip the files into a .zip file using the naming convention 
-- first letter of first name + last name (e.g. mmeagher.zip)


-- PLEASE FILL THIS IN
-- Student Name : Lukas Varga

--SECTION A - 30 Marks

--  In this section replace each "X2 = undefined" with "X2 = fully-parenthesiszed-expression"

-- such that X1 and X2 evaluate to the same value.
-- You may want to consult the table of precedences (see Lab-03)

--- For example I have done "a2" below

a1 = 3 + 5 * 5
a2 = (3 + (5 * 5))


------------------------------
b1 = 5 < 2 + 4
b2 = (5 < (2 + 4))

------------------------------
c1 = length [2] + 5 * 2
c2 = (length [2] + (5 * 2))

-----------------------------
d1 = 5 ^ id 3
d2 = (5 ^ (id 3))

-----------------------------
e1 = [2,3,4] !! 2 ^ 5
e2 = (([2,3,4] !! 2) ^ 5)

-----------------------------
f1 = 3 > 4  &&  2 == 5 * 3
f2 = ((3 > 4)  &&  (2 == (5 * 3)))

------------------------------
g1 = True && 3 `elem` [3,4,5]
g2 = (True && (3 `elem` [3,4,5]))


---------------------------------------------------------------------
-- if when you type "checkAll" you get True, then you have succeeded  with all parts of this section. 

checkAll = all (==True) [a1==a2, b1==b2,c1==c2,d1==d2,e1==e2,f1==f2,g1==g2]




--SECTION B - 30 Marks
-- For each named expression replace "undefined"
-- with an expression with the same type as the declaration


j1:: (String,Integer)
j1 = ("Name", 42)
-- possible answer here would be 
-- j1 = ("Me", 1)

j2:: [Integer]
j2 = [1..10]

j3:: Char
j3 = '#'


j4:: Double
j4 = 42.205


j5:: (Integer,String,Integer,Char)
j5 = (42,"Lukas",42,'@')

j6:: ([Char],(Bool,String))
j6 = (['@','@'],(True,"VSCode"))

j7:: [[Bool]]
j7 = [[True,False],[True,True,True]]

j8:: [(String,Bool)]
j8 = [("Lukas",True),("Dog of Lukas",True)]

