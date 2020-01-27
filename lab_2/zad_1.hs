------------------------------ zad1 ------------------------------
-- Napisac funkcje przestawiajaca elemnty listy w odwrotnym porzadku (bez uzucia reverse)
reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) =  reverse' xs ++ [x]


-- użycie foldl
-- pusta lista to wartosc poczatkowa akumulatora
reverse'2 :: [a] -> [a]
reverse'2 = foldl (\acc x -> x : acc) []


-- użycie foldl
-- pusta lista to wartosc poczatkowa akumulatora
reverse'3 = foldl (flip (:)) [].
