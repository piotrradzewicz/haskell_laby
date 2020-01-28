import Data.Map (fromListWith, toList)
import Data.List
zad2a:: [a] -> [a]
zad2a []=[]
zad2a (x:xs)=[(head xs)] ++ [x] ++ tail xs

zad2b:: [a] -> [a]
zad2b []=[]
zad2b (x:xs)=[last xs] ++ init xs ++[x]

zad2c:: [a] -> [a]
zad2c []=[]
zad2c (x:xs)=[x]++[last (init xs)] ++ tail(init(init xs)) ++ [(head xs)] ++ [last xs]

--zad3

--zad3:: a -> [a] -> Int
zad3 _ []=0
zad3 a b = zad3pom a b 0
zad3pom a (x:xs) e
	|a==x && xs/=[] =zad3pom a xs (e+1)
	|a/=x && xs/=[] =zad3pom a xs e
	|otherwise =e

--zad4

--zad4:: [a] -> [a] -> Bool
zad4 [a] []= False
zad4 [] [a]= False
zad4 [] []= True
zad4 (x:xs) (y:ys)
	|x==y=zad4 xs ys
	|otherwise=False


--zad5

--zad5:: [a] -> [a] -> Bool
zad5 [] []= True
zad5 c d
	|length c /= length d=False
zad5 c d =zad5pom c d
zad5pom (x:xs) d
	|elem x d && xs/=[] = True ==  zad5pom xs d
	|elem x d && xs==[] = True ==  True
	|otherwise=False


--zad6
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs


--zad6:: [a] -> [a] -> [a]
zad6 [] []= []
zad6 [] [a]= quicksort [a]
zad6 [a] []= quicksort [a]
zad6 c d = quicksort(c ++ d)


--zad7
data Tree a = Empty | Node a (Tree a) (Tree a)
t :: Tree Int
t = Node 5 (Node 3 (Node 8 Empty Empty)
           (Node 1 Empty Empty))
    (Node 4 Empty
           (Node 6 Empty Empty)) 

b :: Tree Int
b = Node 3 (Node 8  Empty Empty)
           (Node 1 Empty Empty)

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a l r) = [a] ++ preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a l r) = postorder l ++ postorder r ++ [a]

zad7 _ Empty =False
zad7 Empty _ =False
zad7 q w
	|(zad7c(preorder q)(preorder w)==(preorder w))&&(zad7c(inorder q)(inorder w)==(inorder w))&&(zad7c(postorder q)(postorder w)==(postorder w))=True
	|otherwise=False


zad7c d (x:xs) = takeWhile (/=last xs )(dropWhile (/=x) d) ++ [last xs]


--zad8
--zad8:: [] -> []
--zad8 []= []
--zad8 a = zad8pom a 1 []
--zad8pom (x:xs) e wynik
--xs==[] = wynik
--(x==head xs) && (xs/=[]) = zad8pom (x:tail (filter (/= x) xs)) (e+1) (wynik++[x] ++ [e+1])
--otherwise= zad8pom xs e (wynik ++ [x] ++ [e])
zad8 :: (Ord a) => [a] -> [(a, Int)]
zad8 xs = toList (fromListWith (+) [(x, 1) | x <- xs])

--zad9
--zad9:: [a] -> [a]
--zad9 [] = []
--zad9 (x:xs) = [[x]++[((fst x)^2+(snd x)^2,0)]] ++ zad9 xs


--sortuj (x:xs)
--fst(last x)>fst(head (last xs))=True
--otherwise=False

--zad10
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:xs) = if sorted thisSort then thisSort else bubbleSort thisSort
    where thisSort = (min x y) : bubbleSort ((max x y):xs)

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = if x <= y then sorted (y:xs) else False
