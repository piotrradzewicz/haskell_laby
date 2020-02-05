{- 
Zadanie 1.
Napisać  definicję funkcji obliczającej n-ty wyraz ciągu danego wzorem an=an-1+2an-2 (a1=0,
a2=5) a) rekurencyjną, b) w wersji „akumulatorowej”.
-} 
--(definicja rekurencyjna)
c6z1a a
 | a==1 = 0
 | a==2 = 5
 | otherwise = c6z1a (a-1) + 2*(c6z1a (a-2))

--(definicja „akumulatorowa” - rekurencja ogonowa)

c6z1b a = c6z1bPom a 0 5 

c6z1bPom a x y
 | a == 1 = x
 | a == 2 = y
 | otherwise = c6z1bPom (a-1) y (y + (2*x)) 

{-
c6z1b 3 = c6z1bPom 3 0 5
c6z1bPom 3 0 5 = c6z1bPom 2 5 5
c6z1bPom 2 5 5 = 5

c6z1b 4 = c6z1bPom 4 0 5
c6z1bPom 4 0 5 = c6z1bPom 3 5 5
c6z1bPom 3 5 5 = c6z1bPom 2 5 15
c6z1bPom 2 5 15 = 15


Zadanie 2.
﻿Napisać definicję funkcji, która w liście przestawia a) pierwszy element z drugim, b) pierwszy
element z ostatnim, c) drugi element z przedostatnim.
﻿-}

li1 = [1,2,3,4,4,5,6,7]
li2 = [1,2,3,4,4,5,6,7]
li3 = [7,2,3,4,4,5,6,1]
li4 = [7,2,3,4,5,5,6,1]
li6 = [1,2,3,4,4,5,6,7]
li7 = [1,2,3,4,4,5,6,8]

c6z2a [] = []
c6z2a (x:xs) = head(xs):x:(tail(xs))

c6z2b [] = []
c6z2b (x:xs) = [last (xs)] ++ init(xs) ++ [x] 

c6z2c [] = []
c6z2c (x:xs) = x : [last (init(xs))] ++ init(tail(init(xs))) ++ [head(xs)] ++ [last(xs)]

{-
﻿Zadanie 3.
Napisać  definicję funkcji, której wartością jest liczba wystąpień elementu d w liście l.
Np. f ‘a’ [‘a’,’b’,’a’,’c’,’a’]=3
-}

c6z3 d [] = 0
c6z3 d (x:xs) = length [x|x<-(x:xs), x == d]

{-
﻿Zadanie 4.
Napisać  funkcję sprawdzającą równość dwóch list.
Np.   f [1,2,3,4] [1,2,3,4]=True
f [1,2,3,4] [4,3,2,1]=False
-}

c6z4 [] [] = True
c6z4 (x:xs) [] = False
c6z4 [] (y:ys)= False
c6z4 (x:xs) (y:ys)
 | x==y = c6z4 (xs) (ys)
 | otherwise = False

{- 
﻿Zadanie 5.
Napisać  funkcję sprawdzającą równość dwóch zbiorów.
Np.   f [1,2,3,4] [4,3,2,1]=True
f [1,2,3,4] [5,1,2,3]=False
-}


c6z5 [] [] = True
c6z5 (x:xs) [] = False
c6z5 [] (y:ys)= False
c6z5 (x:xs) (y:ys)
 | sum (c6z5b (x:xs) (y:ys)) == length (y:ys)  = True
 | otherwise = False 



c6z5b (x:xs) (y:ys) = [if c6z5a x (y:ys) == False then 0 else 1|x<-(x:xs)] 
 
c6z5a x [] = False
c6z5a x (y:ys)
 | x == y = True
 | otherwise = c6z5a x ys

{-Zadanie 6.
Napisać  funkcję, która dla dwóch uporządkowanych niemalejąco list liczbowych l1 i  l2  daje w wyniku uporządkowaną niemalejąco listę elementów z list l1 i  l2 . Np.  f [1,3,5,8,10] [0,2,6,9]=[0,1,2,3,5,6,8,9,10]
-}

c6z6 [] [] = []
c6z6 [] (y:ys) = (y:ys)
c6z6 (x:xs) [] = (x:xs)
c6z6 (x:xs) (y:ys)
 |x<=y = x : c6z6 (xs) (y:ys)
 |otherwise = y : c6z6 (x:xs) (ys)
 
{-﻿Zadanie 7.
Napisać  funkcję, która dla dwóch drzew binarnych d1 i d2 zwraca wartość True, gdy drzewo d1 jest poddrzewem drzewa d2.
-}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

t1::Tree Integer
t1 = Node 1(Node 2(Node 4 Empty Empty)(Node 5 Empty (Node 8 Empty Empty)))(Node 3 (Node 6 Empty (Node 9 Empty Empty))(Node 7 Empty Empty))

t2::Tree Integer
t2 = Node 2(Node 4 Empty Empty)(Node 5 Empty (Node 8 Empty Empty))
{- 

c6z7 Empty Empty = False
c6z7 d1 Empty = False
c6z7 Empty d2 = False
c6z7 d1 (Node a l r)
 | r == d1 = True
 | otherwise = False
 

 | r == d1 = True
 | otherwise =  c6z7 d1 l || c6z7 d1 r 
 -}
 

{-﻿Zadanie 8.
Dla danej listy l wypisać listę par [(element, liczba wystąpień), …].
Np. f [2,3,4,1,2,5,3,2,4,4,2 ] = [(2,4),(3,2),(4,3),(1,1),(5,1)]
f                                                                                           [‘a’,’a’,’b’,’a’] = [(‘a’,3),(‘b’,1)]
-} 

li8 = [2,3,4,1,2,5,3,2,4,4,2 ]

licz x [] = 0
licz x (z:zs) 
 | x==z = 1+(licz x zs)
 | otherwise = licz x zs


duplikaty [] y = []
duplikaty (x:xs) y
 | notElem x y = [x]++ duplikaty xs (y++[x])
 | otherwise = duplikaty xs y


c6z8 list1 = [(x,licz x list1)|x<-duplikaty list1 []] 


{-﻿Zadanie 9.
Dla danej listy par (x,y) należy zwrócić listę tych par posortowaną wg odległości od punktu
(0, 0).
Np.  f [(0,3),(1,1),(2,1),(1,0)] = [(1,0),(1,1),(2,1),(0,3)]
-}
li9 = [(0,3),(1,1),(2,1),(1,0)]

c6z9 (x:xs) = c6z9aa (c6z9a (x:xs))

--zwraca liste drugich elementow

c6z9aa [] = []
c6z9aa ((a,b):xs) = b: c6z9aa (xs) 

--uruchamianie sortowania listy z dodanymi kwadratami odleglosci

c6z9a (x:xs) = c6z9c (c6z9b (x:xs)) 0

-- dodawanie odleglosci

c6z9b [] = []
c6z9b ((a,b):xs) = (((a*a)+(b*b)),(a,b)): c6z9b (xs)

-- sortowanie

c6z9c (x:xs) i
 | i == (length(x:xs)) = (x:xs)
 | otherwise = c6z9c (c6z9d (x:xs)) (i+1)

c6z9d ((a,b):(c,d):xs)
 | a > c = (c,d) : c6z9d ((a,b):xs)
 | otherwise = (a,b): c6z9d ((c,d):xs)
c6z9d ((x)) = ((x))


{-
﻿Zadanie 10.
Napisać  funkcję sortującą elementy listy metodą  „bąbelkową”.
-}


c6z10 (x:xs) = c6z10a (x:xs) 0

c6z10a (x:xs) i
 | i == (length(x:xs)) = (x:xs)
 | otherwise = c6z10a (c6z10b (x:xs)) (i+1)

c6z10b (x:y:xs)
 | x > y = y : c6z10b (x:xs)
 | otherwise = x : c6z10b (y:xs)
c6z10b (x) = (x)
 
 


