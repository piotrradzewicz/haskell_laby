{-
----------------------------------------- zad3 -----------------------------------------
  Napisać funkcję dodającą dany element:
    a) na początek listy,
    b) jako drugi element listy,
    c) na koniec listy
-}

----------------------------------------- Rozwiązanie -----------------------------------------
-- Napisać funkcję dodajacą dany elment:
-- a) na poczatek listy

dodajNaPoczatek :: Int -> [Int] -> [Int]
dodajNaPoczatek e l = e : l --operator : konstruuje liste dajac alement jako glowe a reszta jako ogon
-- let l2 = dodajNapoczatek 81 [1,2,3]
-- show l2

-- b) jako drugi elemnet listy
dodajJakoDrugi :: Int -> [Int] -> [Int]
dodajJakoDrugi e [] = []
dodajJakoDrugi e l =  [head l] ++ [e] ++ tail l
--tail l - zwraca cały ogo (bez głowy bedacego pierwszym elementem)
-- head l zwraca pierwszy element ale juz wyłuskany z listy

-- c) na koniec listy
dodajNaKoniec :: Int -> [Int] -> [Int]
dodajNaKoniec e l = l ++ [e] --operator ++ konkatenacji na liscie
