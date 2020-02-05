--coments
{- 
Ćwiczenia 1

Zadanie 1.

Uruchomić WinGHCi lub GHCi i sprawdzić:

2489*981  - 2441709
:t 2489*981  - 2489*981 :: Num a => a
(+) 245 900  - 1145
315^10  - 9618459881658113759765625
:t 315^10  - 315^10 :: Num a => a
(^) 315 10  - 9618459881658113759765625
round 5.4 - 5
:t round(5.4)  - round(5.4) :: Integral b => b
succ 6  - 7
:t succ 6  - succ 6 :: (Enum a, Num a) => a
truncate pi  - 3
:t truncate pi  - truncate pi :: Integral b => b
compare (sqrt 3) (sqrt 6)  - LT
:t compare (sqrt 3) (sqrt 6)  - compare (sqrt 3) (sqrt 6) :: Ordering
(compare 4 5) == EQ  - False
:type (True,”pf”)  - (True,"pf") :: (Bool, [Char])
:type (||)  - (||) :: Bool -> Bool -> Bool
null “abcd”  - False
:t null  - null :: Foldable t => t a -> Bool
:t null “abcd”  - null "abcd" :: Bool
fst(1,’a’)  - 1
fst(‘a’,1)  - 'a'
:t fst(1,’a’)  - fst(1,'a') :: Num a => a
:t fst(‘a’,1)  - fst('a',1) :: Char
:type fst  - fst :: (a, b) -> a
let liczby =[3,4,5]
2:liczby  - [2,3,4,5]
0:1:2:liczby ”OLA”
‘O’:’L’:’A’
“Ala”++”i”++”Ola”
:type “Ala”++”i”++”Ola”
:t sin  - sin :: Floating a => a -> a
:t pi  - pi :: Floating a => a
:t (&&)  - (&&) :: Bool -> Bool -> Bool
True && False  - False
:t True && False  - True && False :: Bool


Zadanie 2.
Napisz funkcję w Haskellu:

a) obliczającą wartość funkcji f(x), gdzie
		x^2	jeśli x >2
f(x) =  x-1, jeśli 0<x<=2
		|x|, jeśli x<=0
-} 
c1z2a x
 | x>2 = x^2
 | x>0 = x-1
 | otherwise = abs x

   
--b) znajdującą największy wspólny dzielnik dwóch liczb

c1z2b x 0 = abs x
c1z2b x y = c1z2b b (mod a b)
 where a = abs x
       b = abs y


--c) znajdującą najmniejszą wspólną wielokrotność dwóch liczb

c1z2c _ 0 = 0
c1z2c 0 _ = 0
c1z2c x y = 
 let mult = (x*y) 
     nwd = c1z2b x y
 in mult `div` nwd


--d) sprawdzającą, czy dla trzech danych długości odcinków można zbudować trójkąt

c1z2d x y z
 | (x < (y+z)) && (y < (x+z)) && (z < (x+y)) = True
 | otherwise = False
 
--e) obliczającą objętość stożka (dane: promień podstawy i wysokość)

c1z2e r h = (1/3) * pi * r * r * h 

--f) obliczającą tworzącą stożka (dane: promień podstawy i wysokość)

c1z2f r h = sqrt((r*r)+(h*h)) 

--g) obliczającą potęgę a^n
--(a≠0, n-liczba naturalna) (definicja rekurencyjna)
c1z2g a n
 | n==0 = 1
 | otherwise = a * c1z2g a (n-1)


--h) obliczającą potęgę a^n
--(a≠0, n-liczba naturalna) (definicja „akumulatorowa” - rekurencja ogonowa)
c1z2h a n = c1z2hPom a n 1

c1z2hPom a n x
 | n==0 = x
 | otherwise = c1z2hPom a (n-1) (a*x)
{--
c1z2h 2 3 = c1z2hPom 2 3 1
c1z2hPom 2 3 1 = c1z2hPom 2 2 2
c1z2hPom 2 2 2 = c1z2hPom 2 1 4
c1z2hPom 2 1 4 = c1z2hPom 2 0 8
c1z2hPom 2 0 8 = 8
--} 
 
 


--i) sprawdzającą, czy dana liczba jest 10-tym elementem ciągu Fibonacciego

c1z2i_fib n 
 | n ==0 = 1
 | n ==1 = 1
 | otherwise = (c1z2i_fib (n-1)) + (c1z2i_fib (n-2))

c1z2i x 
 | x == c1z2i_fib 10 = True
 | otherwise = False

--j) sprawdzającą, czy dana liczba z przedziału <5,100> jest elementem ciągu Fibonacciego 

z = [c1z2i_fib x | x <- [0..11], c1z2i_fib x < 100]

c1z2j y 
 | elem y z = True
 | otherwise = False


--Zadanie 3.
--Napisać funkcję dodającą dany element:
--a) na początek listy,
b = [0,1,2,3,4,5,6,7,8,9,10]

c1z3a y (x:xs) = (y:x:xs)


--b) jako drugi element listy,

c1z3b y (x:xs) = (x:y:xs)

--c) na koniec listy.

c1z3c y (x:xs) = (x:xs) ++[y]


--Zadanie 4.
--Napisać funkcje zwracające:
--a) drugi element listy,
c1z4a [] = []
c1z4a (x:xs) = head (tail (x:xs))

--b) trzeci element listy,
c1z4b [] = []
c1z4b (x:xs) = head (tail (tail(x:xs)))

--c) przedostatni element listy.
c1z4c [] = []
c1z4c (x:xs) = last (init(x:xs))
