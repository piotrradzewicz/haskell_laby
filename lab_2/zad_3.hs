
------------------------------ zad3 ------------------------------
-- Napisz funkcje obliczającą:
-- a) liczbę dodatnich parzystych elemnetów dowolnej listy liczbowej
zliczParzyste :: [Int] -> Int
zliczParzyste [] = 0
zliczParzyste (x:xs) = if even x then 1 + (zliczParzyste xs)
                    else zliczParzyste xs

-- b) Liczbe liczb podizelnych przez 3 z przedzialu <1,n> np. : f(7) = 2
-- uwaga na nawiasy w rekurenbji! bez tego (n-1) bedzie w kółko liczyc od n a nie zmienjszac n
zliczPodzielnePrzez3 :: Int -> Int
zliczPodzielnePrzez3 0 = 0
zliczPodzielnePrzez3 n = if n `mod` 3 == 0 then 1 + (zliczPodzielnePrzez3 (n-1))
                        else zliczPodzielnePrzez3 (n-1)


-- c) sumę licz podizelnych przez 3 z przedzialu <1,n> np. f(7) = 9
sumaPodzielnePrzez3 :: Int -> Int
sumaPodzielnePrzez3 0 = 0
sumaPodzielnePrzez3 n = if n `mod` 3 == 0 then n + (sumaPodzielnePrzez3 (n-1))
                      else sumaPodzielnePrzez3 (n-1)
