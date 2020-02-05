--Ćwiczenia 2
--Zadanie 1.
--Napisać funkcję przestawiającą elementy listy w odwrotnym porządku (bez użycia --reverse)

c2z1 [] = []
c2z1 (x:xs) = (c2z1 xs) ++ [x]

--Zadanie 2.
--Napisać funkcję przestawiającą w liście ostatni element z pierwszym.

c2z2  [] = []
c2z2  (x:xs) = [last (xs)] ++ init(xs) ++ [x] 

--Zadanie 3.
--Napisać funkcję obliczającą:
--a) liczbę dodatnich parzystych elementów dowolnej listy liczbowej,

c2z3a [] = 0
c2z3a (x:xs) = length [x|x<-(x:xs), even x && x>0 ]

--b) liczbę liczb podzielnych przez 3 z przedziału <1,n>, np. f(7)=2,

c2z3b n = length [x|x<-[1..n], mod x 3 == 0 ]


--c) sumę liczb podzielnych przez 3 z przedziału <1,n>, np. f(7)=9.

c2z3c n = sum [x|x<-[1..n], mod x 3 == 0 ]


--Zadanie 4.
--Napisać funkcję sprawdzającą, czy lista ma parzystą liczbę elementów.

c2z4 (x:xs)
 |mod (length (x:xs)) 2 == 0 = True
 |otherwise = False

--Zadanie 5.
--Zdefiniować funkcję podnoszącą do kwadratu wszystkie elementy danej listy liczb
--całkowitych, np. sqrlist [1,2,-3] zwraca [1, 4, 9].
--a) z użyciem map,
pom x = x*x
c2z5a (x:xs) = map pom (x:xs)

--b) bez użycia map.

c2z5b (x:xs) = [x*x|x<-(x:xs)]

c2z5b1 [] = []
c2z5b1 (x:xs) = (x^2 : c2z5b1 xs)


--Zadanie 6.
--Zdefiniować polimorficzną funkcję obliczającą, ile razy dany obiekt występuje w danej liście, np. count('a', ['a', 'l', 'a']) zwraca 2. Jaki jest typ tej funkcji? 

c2z6 z (x:xs) = length [x|x<-(x:xs), x == z]
--Eq a => a -> [a] -> Int

--Zadanie 7.
--Zdefiniować polimorficzną funkcję powtarzającą dany obiekt określoną liczbę razy
--i zwracającą wynik w postaci listy, np. duplicate("ppd",3) zwraca ["ppd","ppd","ppd"]. Jaki jest typ tej funkcji?

c2z7 x 0 = []
c2z7 x n = x : c2z7 x (n-1)
--(Eq t, Num t) => a -> t -> [a]

--Zadanie 8.
--Zdefiniować polimorficzną funkcję sprawdzającą, czy dana lista jest palindromem, --tj. równa się sobie samej przy odwróconej kolejności elementów.

rev [] = []
rev (x:xs) = (c2z1 xs) ++ [x]

c2z8 (x:xs) = (x:xs) == rev(x:xs)


--Zadanie 9.
--Zdefiniować polimorficzną funkcję usuwającą pierwsze wystąpienie danego elementu w liście.

c2z9 a [] = []
c2z9 a (x:xs) 
 | a == x = xs
 | otherwise = x : c2z9 a xs

--Zadanie 10.
--Zdefiniować polimorficzną funkcję usuwającą element na n-tym miejscu w liście.

c2z10 0 (x:xs) = xs -- usuniecie glowy
c2z10 _[]=[] -- gdy dzialamy na pustej liscie
c2z10 n (x:xs) = take n (x:xs) ++ drop (n+1) (x:xs) 


--Zadanie 11.
--Napisać definicję dwuargumentowej funkcji określonej dla list, której wartością jest prawda, jeśli wszystkie elementy pierwszej listy występują na drugiej liście.

c2z11a x [] = False
c2z11a x (y:ys)
 | x == y = True
 | otherwise = c2z11a x ys
 
c2z11b (x:xs) (y:ys) = [if c2z11a x (y:ys) == False then 1 else 0|x<-(x:xs)] 

c2z11 [] [] = True
c2z11 [] (y:ys) = True
c2z11 (x:xs) (y:ys)
 | sum (c2z11b (x:xs) (y:ys)) == 0 = True
 | otherwise = False

--Zadanie 12.
-- Napisać definicję funkcji, której argumentem jest lista krotek 2-elementowych, a wartością lista krotek z przestawionymi elementami, np. wartością zamiana [(1,’a’),(2,’b’)] jest [(’a’,1),( ’b’,2)].

pom12 x = (snd x, fst x)
c2z12 s = map pom12 s