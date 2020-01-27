------------------------------ zad9 ------------------------------
-- Zdefiniowac polimorficzna funkcje usuwająca pierwsze wystapienie
 -- danego elemntu w liscie
 usunPierwszeWystapienie :: Eq t => t -> [t] -> [t] -- elment e i listy musza byc tego samego typu, i ten typ musi byc implementowac porowywannie
 usunPierwszeWystapienie e [] = []
 usunPierwszeWystapienie e (x:xs) = if e == x then xs else [x] ++ (usunPierwszeWystapienie e xs)
