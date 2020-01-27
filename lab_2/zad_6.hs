------------------------------ zad6 ------------------------------
-- Zdefiniowac polimorficzna funkcje obliczajaca ile razy dany obiket wystepuje w danej liscie, np. count ('a', ['a', 'l', 'a']) zwraca 2
-- jaki jest type tej funkcji?
-- Eq - klasa którą da się porównywać między sobą
zlicz :: (Num p, Eq t) => t -> [t] -> p
zlicz e [] = 0
zlicz e (x:xs) = if e == x then 1 + (zlicz e xs) else zlicz e xs

-- *Main> :type zlicz
-- zlicz :: (Num p, Eq t) => t -> [t] -> p
