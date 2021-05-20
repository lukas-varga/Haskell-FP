-- Name: Lukas Varga

-- This question is worth 30 Marks

--  In this section replace each "X2 = undefined" with "X2 = fully-parenthesiszed-expression"

-- such that X1 and X2 evaluate to the same value.

-- You may want to consult the table of precedences (see Lab-03)

--- For example I have done "a2" below

a1 = 3 + 5 * 5

a2 = (3 + (5 * 5))

------------------------------

b1:: Bool

b1 = 10 == 12 + 4

b2 = (10 == (12 + 4))

------------------------------

c1 = head [12,2,3] + 2 ^ 6

c2 = ((head [12,2,3]) + (2 ^ 6))

-----------------------------

d1 = 5 ^ head [3,3,4]

d2 =  (5 ^ (head [3,3,4]))

-----------------------------

e1 = length [2,3,4]  ^ 2*3

e2 = (((length [2,3,4])  ^ 2)*3)

-----------------------------

f1 = 3 >= 4  &&  15 > 5 * 3

f2 = ((3 >= 4)  &&  (15 > (5 * 3)))

------------------------------

g1 = False  && 3 < 4 `div`3    

g2 = (False  && (3 < (4 `div`3)))  

---------------------------------------------------------------------

-- if when you type "checkAll" you get True, then you have succeeded  with all parts of this section. 

checkAll = all (==True) [a1==a2, b1==b2,c1==c2,d1==d2,e1==e2,f1==f2,g1==g2]