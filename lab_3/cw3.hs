--Ćwiczenia 3
--Zadanie 1.
--Napisać 6 definicji różnych (dowolnych) funkcji z wykorzystaniem wyrażeń lambda. --Podać przykładowe obliczenia wartości tych funkcji.
--Np. f list = map (\x -> x+3) list
--f [1,2,3] = [4,5,6]

d = ["apples", "oranges", "mangos"]

c3z1a (x:xs) =  map (\(x:xs) -> 'c':x:xs) (x:xs)
--["capples","coranges","cmangos"]

pom_c3z1a (x:xs) = ('c':x:xs)
c3z1a1 = map pom_c3z1a d

c3z1b (x:xs) = map (\x -> x*x) (x:xs)
-- c3z1b [1,2,3] = [1,4,9]

c3z1c (x:xs) = foldr1 (\x y -> 2*x + y) (x:xs)
-- c3z1b [1,2,3] = 9 

c3z1d (x:xs) = foldr (\x y -> (x+y)/2) 5 (x:xs)
-- c3z1d [1,2,3] = 2.0

c3z1e (x:xs) z = foldr (\x y -> x+y) z (x:xs)
-- c3z1e [1,2,3] = 11

c3z1f (x:xs) (y:ys) = zipWith (\x y -> 2*x + y) (x:xs) (y:ys)
--c3z1f [1,2,3] [5,3,4] = [7,7,10]


{-
﻿Zadanie 2.
Zdefiniować typ o nazwie moto, którego konstruktorami są nazwy marek samochodów
(5 różnych marek). Napisz definicje:
a)  funkcji, która nazwie państwa przypisuje jedną markę samochodu produkowanego
w tym państwie, dla państwa zdefiniować synonim typu [Char] o nazwie Kraj.
b)  funkcji, która danej marce samochodu przypisuje średnią prędkość, jaką może osiągać
auto tej marki.
-}

data Moto = Mazda|Honda|Volkswagen|Audi|Renault deriving (Show)
type Kraj = [Char]

kraje:: Moto -> Kraj
kraje m = case m of
 Mazda -> "Japonia"
 Honda -> "Japonia"
 Volkswagen -> "Niemcy"
 Audi -> "Niemcy"
 Renault -> "Francja"

srednaipredkosc:: Moto -> Float
srednaipredkosc m = case m of
 Mazda -> 30
 Honda -> 40
 Volkswagen -> 32
 Audi -> 39
 Renault -> 29


{-﻿Zadanie 3.
Napisać definicję poniżej podanych drzew.
Sprawdzić działanie funkcji preorder, postorder i inorder przedstawionych na wykładzie dla
następujących drzew:-}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
ta::Tree Integer

ta = Node 1(Node 2(Node 4 Empty Empty)(Node 5 Empty (Node 8 Empty Empty)))(Node 3 (Node 6 Empty (Node 9 Empty Empty))(Node 7 Empty Empty))

tb:: Tree Char

tb = Node 'a'(Node 'b' Empty (Node 'd' (Node 'f' Empty Empty) Empty)) (Node 'c' (Node 'e' Empty (Node 'g' Empty Empty)) Empty) 

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a l r) = [a] ++ preorder l ++  preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a l r) = postorder l ++ postorder r ++ [a]

--preorder ta = [1,2,4,5,8,3,6,9,7]
--postorder ta = [4,8,5,2,9,6,7,3,1]
--inorder ta = [4,2,5,8,1,6,9,3,7]

--preorder tb = "abdfceg"
--postorder tb = "fdbgeca"
--inorder tb = "bfdaegc"


{-
Zadanie 4.
Zdefiniować funkcję tree-member, która sprawdza przynależność elementu do drzewa.-}

--a)  z wykorzystaniem funkcji elem

treemembera x t = elem x (inorder t)

--b)  bez wykorzystania funkcji elem.

pom4 x [] = False
pom4 x (y:ys)
 | x == y = True
 | otherwise = pom4 x ys

treememberb x t = pom4 x (inorder t)

--Zadanie 5.
--Napisać funkcję, której wartością jest długość a) najdłuższej, b) najkrótszej gałęzi w drzewie binarnym.

c3z5a :: Tree a -> Integer
c3z5a Empty = 0
c3z5a (Node a l r) = 1 + (max (c3z5a l) (c3z5a r))

c3z5b :: Tree a -> Integer
c3z5b Empty = 0
c3z5b (Node a l r) = 1 + (min (c3z5b l) (c3z5b r)) 


{-
Zadanie 6.
Zdefiniować funkcję poziomo przeglądania elementów (wierzchołków) drzewa binarnego
poziomami (według strategii „wszerz”), czyli: korzeń, korzenie poddrzew pierwszego
poziomu, korzenie poddrzew drugiego poziomu itd.
Np. dla drzewa z Zadania 1 a): poziomo t = [1,2,3,4,5,6,7,8,9]
-}

pom6 [] = []
pom6 (Empty:xs) = pom6 xs
pom6 (Node a l r:xs) = a: pom6 (xs ++ [l, r]) 

levelorder x = pom6 [x]
