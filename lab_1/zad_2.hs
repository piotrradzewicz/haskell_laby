{-
----------------------------------------- zad2 -----------------------------------------
  Napisz funkcję w Haskellu:
    a) obliczającą wartość funkcji f(x), gdzie:
                  | x^2 , jeśli x >2
          f(x) = {  x -1, jeśli 0 < x <= 2
                  | |x|, jeśli x <= 0
    b) znajdującą największy wspólny dzielnik dwóch liczb
    c) znajdującą najmniejszą wspólną wielokrotność dwóch liczb
    d) sprawdzającą, czy dla trzech danych długości odcinków można zbudować trójkąt
    e) obliczającą objętość stożka (dane: promień podstawy i wysokość)
    f) obliczającą tworzącą stożka (dane: promień podstawy i wysokość)
    g) obliczającą potęgę an
    (a≠0, n-liczba naturalna) (definicja rekurencyjna)
    h) obliczającą potęgę an
    (a≠0, n-liczba naturalna) (definicja „akumulatorowa” -
    rekurencja ogonowa)
    i) sprawdzającą, czy dana liczba jest 10-tym elementem ciągu Fibonacciego
    j) sprawdzającą, czy dana liczba z przedziału <5,100> jest elementem ciągu
    Fibonacciego
-}

----------------------------------------- Rozwiązanie -----------------------------------------
-- a)
-- import Data.Maybe
-- f :: Num x => x -> Maybe x
-- f x
--   | x <= 0  = abs (x)
--   | 0 < x && x <= 2 = x -1
--   | x > 2 = x
--   | otherwise = Nothing

-- TODO: Zobacyzć z Maybe - Haskell Monad
f x
  | x <= 0  = abs (x)
  | 0 < x && x <= 2 = x -1
  | otherwise  = x*x

--------------------------------------------------
--  b) Najwiekszy wspolny dzielnik
-- algorytm euklidesa
gcd' x 0 = abs x
gcd' x y = gcd' b (mod a b)
  where a = abs x
        b = abs y

-- c) Najmniejsza wspola wielokrotnosc
lcm' :: Int -> Int -> Int
lcm' 0 0 = 0
lcm' x y = abs ((x * y) `div` (gcd' x y )) -- div zamiast dizelenia \ bo dzialamy na intach

-- d) Sprawdzanie czy mozna zbudowac trojkat z odcinkow
--  suma 2 bokow musi byc wieksza od jendego z bokow
trojkat :: Int -> Int -> Int -> Bool
trojkat a b c
  | (a + b) > c && (a + c) > b && (b + c) > a = True
  | otherwise = False

-- e) obliczajaca objetosc stozka - promien podtsawy i wysokosc
stozekObjetosc r h =1/3*pi*r^2*h

-- f) obliczajaca tworzaca stozka - promien podstawy i wysokosc
stozekTworzaca r h = sqrt(h^2 + r^2)

-- g) obliczajaca potehge a^n (a!= 0 i n liczba naturalna) def rekurencyjna
potega a 0 = 1
potega a n = a * potega a (n-1)


-- h) potega definicja akumulatorowa - iteracyjna
potegaAkumulator a n = akumulator a n 1 -- 1 to wartosc przechowywanana akumulator to x
akumulator a n x = if n == 0 then x -- n w sumie jest jedynym warunkiem zakonczenia
            else akumulator a (n-1) (a*x) -- akumulator to x i jest przemnonozony; iteracja n kjest zmnjeijszane o 1

-- i) sprawdzajaa cyz jest 10-tym elementem ciągu fibbonchiego
fib 1 = 1
fib 2 = 1
fib n = (fib (n-1)) + (fib (n-2))

isFib10 x
  | x == (fib 10) = True
  | otherwise = False


-- j) sprawdzajaca czy liczba z przedzialu < 5 , 100 > jest elementem ciagu fibbonchiego
-- TODO: Zobacyzć z Maybe - Haskell Monad
isFibNumber :: Int -> Bool
isFibNumber x
              | x < 5  = False
              | x > 100 = False
              | x `elem` fibNumbers = True
              | x `notElem` fibNumbers = False
              where fibNumbers = [fib x | x <- [1..20],  fib x > 5 && fib x <= 100]


isFibNumber2 :: Int -> Bool
isFibNumber2 x = if x `elem` fibNumbers then True else False
                  where fibNumbers = [fib x | x <- [1..20],  fib x > 5 && fib x <= 100]
