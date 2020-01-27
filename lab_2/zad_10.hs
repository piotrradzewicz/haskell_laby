------------------------------ zad10 ------------------------------
-- Zdefiniowac polimorficzną funkcję usuwającą elment na n-tym miejscu w liscie.
usunNty :: [a] -> Int -> [a]
usunNty [] n = []
usunNty l n = if dlugosc >= n then take (n-1) l ++ drop  (n + 1) l else l
            where dlugosc = length l
