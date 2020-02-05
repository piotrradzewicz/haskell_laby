--zadanie 2
-- podpunkt a
pkta:: Int -> Int
pkta x
        |x>2=x^2
        |(0<x && x<=2) = x-1
        |x<=0=x*(-1)

-- podpunkt b
nwd:: Int -> Int -> Int
nwd a b
	| b == 0    = a
	| otherwise = nwd b (a `mod` b)

-- podpunkt c
nww:: Int -> Int -> Int 
nww x y= abs (x `quot` (gcd x y) * y)

-- podpunkt d
trojkat:: Int -> Int -> Int -> String
trojkat a b c
	| (a+b>c && b+c>a && c+a>a) = "jest trojkatem"
	| otherwise="nie jest trojkatem"

-- podpunkt e
stozek :: Double-> Double-> Double
stozek r h =0.33 * 3.14 * h *r^2

-- podpunkt f
tworzacaStozek :: Double-> Double-> Double
tworzacaStozek r h =0.33 * 3.14 * h *r^2

-- podpunkt g
potegaRek :: Int -> Int -> Int
potegaRek a n  
	| (n==1)=a
	| otherwise=potegaRek(a)(n-1)*a

-- podpunkt h
potegaAku :: Int -> Int -> Int
potegaAkuPom  :: Int -> Int -> Int -> Int
potegaAku a n = potegaAkuPom a n 1
potegaAkuPom a n x
	| (n==0)=x
	| otherwise=potegaAkuPom(a)(n-1)(a*x)

-- podpunkt i
fib n = fibPOM n 1 1
fibPOM n f1 f2 = if n==1 then f1
else fibPOM (n-1) (f1+f2) f1
sprawdzanieFib :: Int -> String
sprawdzanieFib a
	|(fib(10)==a) ="tak"
	|otherwise ="Nie"


-- podpunkt j
ciag=[5,8,13,21,34,55,89]
fib_ciag:: Int ->  Bool
fib_ciag x
	|x `elem` ciag = True
	|otherwise =False




-- zadanie 3
ciag2=[5,8,13,21,34,55,89]

-- podpunkt a
lista_pocz:: [Int] -> Int -> [Int]
lista_pocz y x = (x:y)

-- podpunkt b
init1 :: Int -> [Int] -> [Int]
init1 b (x:xs) = (x:[b]++xs)

-- podpunkt c
init2 :: Int -> [Int] -> [Int]
init2 b (x:xs) = (x:xs++[b])



-- zadanie 4

-- podpunkt a
zad4a :: [Int] -> Int
zad4a (x:xs) = last(take 2 (x:xs))

-- podpunkt b
zad4b :: [Int] -> Int
zad4b (x:xs) = last(take 3 (x:xs))


-- podpunkt c
zad4c :: [Int] -> Int
zad4c (x:xs) = head(drop(length(x:xs)-2) (x:xs))




