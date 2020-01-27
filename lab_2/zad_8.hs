------------------------------ zad8 ------------------------------
-- Zdefiniować polimorficzną funkcję sprawdzającą czy dana lista jest palindromem,
 -- tj równa sobie same przy odwróconej kolejności elementów
-- palindrom :: [Char] -> Bool
palindrom :: Eq a => [a] -> Bool
palindrom [] = True
palindrom [x] = True --moze byc nieparzysa liczba jak w kajak j jest porownywane same ze soba
palindrom l = if head l == last l then palindrom (init( tail l)) else False
