{-﻿Zadanie 1.
Napisać moduł Mojzbior z funkcjami podzbior, iloczyn, suma,  roznica  (podzbiór,
iloczyn zbiorów, suma zbiorów, różnica zbiorów odpowiednio).
-}

import Mojzbior

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
ta::Tree Integer
ta = Node 1(Node 2(Node 4 Empty Empty)(Node 5 Empty (Node 8 Empty Empty)))(Node 3 (Node 6 Empty (Node 9 Empty Empty))(Node 7 Empty Empty))

{-
﻿Zadanie 2.
Napisać  funkcję, która dla drzewa binarnego d zwraca True, gdy drzewo ma w węzłach
liczby całkowite i wszystkie elementy w lewym poddrzewie są mniejsze od liczby
w korzeniu, a w prawym poddrzewie - większe.
-}

tree2 Empty = True
tree2 (Node a l r)
 | a /= fromInteger (a) = False
 | otherwise = tree2 l && tree2 r 
 
node (Node a l r) = a

tree3 Empty = True
tree3 (Node a l r)
 | (node l > a) && (node r < a)= tree3 l && tree3 r 
 | otherwise = False


{-Zadanie 3.
Dla danej listy l wypisać listę par [(element, liczba wystąpień), …].
Przykład: w [2,3,4,1,2,5,3,2,4,4,2 ] = [(2,4),(3,2),(4,3),(1,1),(5,1)]
-}


count x [] = 0
count x (y:ys) | x==y = 1+(count x ys)
               | otherwise = count x ys

rmdups [] = []
rmdups (x:xs)   | elem x xs   = rmdups xs
                | otherwise     = x : rmdups xs


c4z3 list1 = rmdups[(x,(count x list1))|x<-list1] 

{-Zadanie 4.
Funkcja zip tworzy listę krotek z elementów list wejściowych, unzip działa odwrotnie.
zip   :: [a] -> [b] -> [(a,b)]
unzip :: [(a, b)] -> ([a], [b])
-}


zipp [] _ = []
zipp _ [] = []
zipp (x:xs) (y:ys) = (x,y):zipp xs ys

unzipp [] = ([], [])
unzipp xs = (map fst xs, map snd xs)


{-﻿Zadanie 5.
Zapisać kolejność obliczeń wartości każdej z funkcji (przykład na wykładzie):


foldr (/) 2 [6, 12, 24, 8]
3.0
==> 6 / (foldr (/) 2 [12, 24, 8]
==> 6 / (12 / foldr (/) 2 [24, 8])
==> 6 / (12 / (24 / foldr (/) 2 [8]))
==> 6 / (12 / 24 / (8 / foldr (/) 2 []))
==> 6 / (12 / (24 / (8 / 2)))
==> 6 / (12 / (24 / 4))
==> 6 / (12 / 6)
==> 6 / 2
==> 3

foldr (&&) True [1>2, 3>2, 5==5]
False
==> 1>2 && (foldr (&&) True [3>2, 5==5])
==> 1>2 && (3>2 && foldr (&&) True [5==5])
==> 1>2 && (3>2 && (5==5 && foldr (&&) True []))
==> 1>2 && (3>2 && (5==5 && True))
==> 1>2 && (3>2 && True)
==> 1>2 && True
==> False

foldr max 18 [3, 6, 12, 4, 55, 11]
55
==> 3 max (foldr max 18 [6, 12, 4, 55, 11])
==> 3 max (6 max (foldr max 18 [12, 4, 55, 11]))
==> 3 max (6 max (12 max (foldr max 18 [4, 55, 11])))
==> 3 max (6 max (12 max (4 max (foldr max 18 [55, 11]))))
==> 3 max (6 max (12 max (4 max (55 max (foldr max 18 [11])))))
==> 3 max (6 max (12 max (4 max (55 max (11 max foldr 18 [])))))
==> 3 max (6 max (12 max (4 max (55 max (11 max 18)))))
==> 3 max (6 max (12 max (4 max (55 max 18))))
==> 3 max (6 max (12 max (4 max 55)))
==> 3 max (6 max (12 max 55))
==> 3 max (6 max 55)
==> 3 max 55
==> 55

foldr max 81 [3, 6, 12, 4, 55, 11]
81
==> 3 max (foldr max 81 [6, 12, 4, 55, 11])
==> 3 max (6 max (foldr max 81 [12, 4, 55, 11]))
==> 3 max (6 max (12 max (foldr max 81 [4, 55, 11])))
==> 3 max (6 max (12 max (4 max (foldr max 81 [55, 11]))))
==> 3 max (6 max (12 max (4 max (55 max (foldr max 81 [11])))))
==> 3 max (6 max (12 max (4 max (55 max (11 max (foldr max 81 []))))))
==> 3 max (6 max (12 max (4 max (55 max (11 max 81)))))
==> 3 max (6 max (12 max (4 max (55 max 81))))
==> 3 max (6 max (12 max (4 max 81)))
==> 3 max (6 max (12 max 81))
==> 3 max (6 max 81)
==> 3 max 81
==> 81

foldr (\x y -> (x+y)/2) 54 [24, 4, 10, 6]
==> 24 (\x y -> (x+y)/2) (foldr (\x y -> (x+y)/2) 54 [4, 10, 6])
==> 24 (\x y -> (x+y)/2) (4 (\x y -> (x+y)/2) (foldr (\x y -> (x+y)/2) 54 [10, 6]))
==> 24 (\x y -> (x+y)/2) (4 (\x y -> (x+y)/2) (10 (\x y -> (x+y)/2) (foldr (\x y -> (x+y)/2) 54 [6])))
==> 24 (\x y -> (x+y)/2) (4 (\x y -> (x+y)/2) (10 (\x y -> (x+y)/2) (6 (\x y -> (x+y)/2) (foldr (\x y -> (x+y)/2) 54 []))))
==> 24 (\x y -> (x+y)/2) (4 (\x y -> (x+y)/2) (10 (\x y -> (x+y)/2) (30)
==> 24 (\x y -> (x+y)/2) (4 (\x y -> (x+y)/2) (20)
==> 24 (\x y -> (x+y)/2) (12)
==> 18

foldl (\x y -> (x+y)/2) 54 [2, 4, 10, 6]
==> foldl (\x y -> (x+y)/2) (54 (\x y -> (x+y)/2) 2) [4, 10, 6]
==> foldl (\x y -> (x+y)/2) ((54 (\x y -> (x+y)/2) 2) (\x y -> (x+y)/2) 4) [10, 6]
==> foldl (\x y -> (x+y)/2) (((54 (\x y -> (x+y)/2) 2) (\x y -> (x+y)/2) 4) (\x y -> (x+y)/2) 10) [6]
==> foldl (\x y -> (x+y)/2) ((((54 (\x y -> (x+y)/2) 2) (\x y -> (x+y)/2) 4) (\x y -> (x+y)/2) 10) (\x y -> (x+y)/2) 6) []
==> (( 28 (\x y -> (x+y)/2) 4) (\x y -> (x+y)/2) 10) (\x y -> (x+y)/2) 6
==> (16 (\x y -> (x+y)/2) 10) (\x y -> (x+y)/2) 6
==> 13 (\x y -> (x+y)/2) 6
==> 9.5

foldl (/) 64 [4, 2, 4]
==> foldl (/) (64 / 4) [2, 4]
==> foldl (/) ((64 / 4) / 2) [4]
==> foldl (/) (((64 / 4) /2) /4) []
==> ((16) / 2) / 4
==> 8 / 4
==> 2

foldl (\x y -> 2*x + y) 8 [1, 2, 3]
==> foldl (\x y -> 2*x + y) (8 (\x y -> 2*x + y) 1) [2, 3]
==> foldl (\x y -> 2*x + y) ((8 (\x y -> 2*x + y) 1) (\x y -> 2*x + y) 2) [3]
==> foldl (\x y -> 2*x + y) (((8 (\x y -> 2*x + y) 1) (\x y -> 2*x + y) 2) (\x y -> 2*x + y) 3) []
==> ((17 (\x y -> 2*x + y) 2) (\x y -> 2*x + y) 3)
==> (36 (\x y -> 2*x + y) 3)
==> 75
-}





