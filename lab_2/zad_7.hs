------------------------------ zad7 ------------------------------
-- zdefiniować polimorficzną funkcję powtarzajacą dany obiekt określoną liczbę
-- razy i zwracającą wynik w postaci listy , np. duplicate("ppd", 3) zwraca ["ppd", "ppd", "ppd"]
-- jaki jest typ tej funkcji

duplicate :: (Eq t, Num t) => a -> t -> [a]
duplicate pattern 0 = [] -- w przypadku (-1) mamy nieskonczonosc
duplicate pattern n = [pattern] ++ duplicate pattern (n-1)

duplicate2 :: (Ord t, Num t) => a -> t -> [a]
duplicate2 pattern n = if n > 0 then [pattern] ++ (duplicate pattern (n-1)) else []

duplicate3 :: (Ord t, Num t) => a -> t -> [a]
duplicate3 pattern n
                      |n > 0 = [pattern] ++ duplicate pattern (n-1)
                      | otherwise  = []
