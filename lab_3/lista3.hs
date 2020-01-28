--Zadanie1
f list = map (\x -> 4) list
g list = map (\x -> x+5) list
h list = map (\x -> x+6) list
i list = map (\x -> -3) list
j list = map (\x -> 6)list
k list = map (\x -> x*2) list


--Zadanie2
--Podpunkt A
data Moto = Ford | Fiat | Nissan | Skoda | BMW
	deriving (Show)
kraj:: [Char]-> Moto
kraj x
	| x=="Anglia" = Ford 
	| x=="Rosja" = Fiat 
	| x=="Niemcy" = Nissan 
	| x=="Polska" = Skoda 
	| x=="Włochy" = BMW


--Podpunkt B
predkosc:: [Char]-> Moto
predkosc y
	| y=="90" = Ford 
	| y=="77" = Fiat 
	| y=="88" = Nissan 
	| y=="99" = Skoda 
	| y=="89" = BMW



--Zadanie3
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a l r) = [a] ++ preorder l ++ preorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a l r) = postorder l ++ postorder r ++ [a]

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

--Podpunkt A
data Tree a = Empty | Node a (Tree a) (Tree a)
t :: Tree Int
t = Node 1 (Node 2 (Node 4 Empty Empty)
	(Node 5 Empty (Node 8 Empty Empty)))
	(Node 3 (Node 6 Empty (Node 9 Empty Empty))(Node 7 Empty Empty))

--Podpunkt A
--data Tree b = Empty | Node b (Tree b) (Tree b)
--tr :: Tree Char
--tr = Node 'a' (Node 'b' Empty (Node 'd' (Node 'f' Empty Empty) Empty))
	--(Node 'c' (Node 'e' Empty (Node 'g' Empty Empty))Empty)



--Zadanie4
--Podpunkt A
tree2 :: Int -> Bool
tree2 x = elem x (inorder t)

--Podpunkt B 
tree3 :: Int -> Bool
tree3 z = pom1 z (inorder t)
pom1 z (x:xs)
	|(x==z)=True
	|(xs==[])=False
	|otherwise=(pom1 z xs)


--Zadanie5
--Podpunkt A
depth :: Tree a -> Int
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l)(depth r)

--Podpunkt B
depthmin :: Tree a -> Int
depthmin Empty = 0
depthmin (Node _ l r) = 1 + min(depthmin  l)(depthmin  r)

--Zadanie6
--zadanie6 :: Tree a -> [a]
--zadanie6 Empty = []
--zadanie6 (Node a (Node a1 l1 r1) (Node a2 l2 r2)) = [a] ++ [a1] ++[a2] ++ zadanie6 l1++ zadanie6 r1

--cwiczenie kolo
a = do 
	x <- getChar
	getChar
	y <- getChar
	return (x,y)

main = do
putStrLn "Podaj imie:"
imie <- getLine
putStrLn ("Witaj" ++ imie)




	




