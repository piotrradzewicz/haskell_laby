------------------------------ zad1 ------------------------------
-- Napisać6 definicji różnych (downolnych) funckji z wykoryztsaniem wyrażeń lambda.
-- Podac przkładowe obliczenia wartości tych funckji
-- Np. f list = map (\x -> x + 3) list
-- f[1,2,3] = [4,5,6]

f = map (+3) [1,5,6,3] --currying
f2 =  map (\x -> x+3) [1,5,6,3]

----------- przykład 1.
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))


----------- przykład 2.
-- lambda dwa parametry
p2 :: [Double]
p2 = zipWith (\a b -> (a*30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
-- [153.0,61.5,31.0,15.75,6.6]

----------- przykład 3.
-- lambda dwa parametry
p3 :: [Integer]
p3 = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]


----------- przykład 4.
-- lambda trzy parametry
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree2 :: (Num a) => a -> a -> a -> a  -- nieczytelne - w gruncie rzeczy ilustracja currying
addThree2 = \x -> \y -> \z -> x + y + z


----------- przykład 5.
-- lambda dwa parametry; ilustracja flip bedacego zamiana kolejnosci parametrow
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x


----------- przykład 6.
-- lambda jeden parametr; innadefinicja funckji
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs --acc to akumulator który przyjmuje wartosc stary akumulator + x; 0 to poczatkowa wartosc akumulatora
-- (\acc x -> acc + x) to to samo co (+)

sum'2 :: (Num a) => [a] -> a
sum'2 = foldl (+) 0


----------- przykład 7.
-- lambda jeden parametr
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys
