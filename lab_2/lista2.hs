--cwiczenia 2
--zadnie 1
funkcja3 :: [a] -> [a] 
funkcja3 [] =[]
funkcja3 a = [last a] ++ funkcja3 (init a)



--zadnie 2
funkcja :: [a] -> [a] 
funkcja a= [last a] ++ (funkcja3 a) ++ [head a]



--zadnie 3

-- podpunkt a

zad1C2 :: [Int] -> Int
zad1C2 (x:xs) = zad1C2POM (x:xs) 0
zad1C2POM (x:xs) n
	|(x`mod`2==0 && length xs/=0 && x>0)=zad1C2POM(xs)(n+1)
		|(x`mod`2==0 && length xs==0 && x>0)=n+1
			|(x`mod`2/=0 && length xs==0)=n
	|otherwise=zad1C2POM(xs)(n)


-- podpunkt b
zad1bC2 :: Int -> Int
zad1bC2 x = zad1bC2POM [1..x] 0
zad1bC2POM (x:xs) n
	|(x`mod`3==0 && length xs/=0)=zad1bC2POM(xs)(n+1)
		|(x`mod`3==0 && length xs==0)=n+1
			|(x`mod`3/=0 && length xs==0)=n
	|otherwise=zad1bC2POM(xs)(n)

-- podpunkt c
zad1cC2 :: Int -> Int
zad1cC2 y = zad1cC2POM [1..y] 0
zad1cC2POM (x:xs) suma
	|(x`mod`3==0 && length xs/=0)=zad1cC2POM(xs)(suma+x)
		|(x`mod`3==0 && length xs==0)=suma+x
			|(x`mod`3/=0 && length xs==0)=suma
	|otherwise=zad1cC2POM(xs)(suma)



-- zadnie 4
zad4C2 :: [Int] -> Bool
zad4C2 x = length x `mod` 2==0




-- zadnie 5
-- podpunkt a
zad5aC2 :: [Int] -> [Int]
zad5aC2 x =  map (\a->a^2) x


-- podpunkt b
zad5bC2 :: [Int] -> [Int] 
zad5bC2 (x:xs) = zad5bC2POM (x:xs) []
zad5bC2POM (x:xs) b
	|(length (xs)==0)=b++[x^2]
	|otherwise=zad5bC2POM(xs)(b++[x^2])


-- zadnie 6
zad6C2 :: Eq a => a -> [a] -> Int
zad6C2 c (x:xs) = zad6C2POM c (x:xs) 0
zad6C2POM c (x:xs) licznik
	|(c==x && length (xs)/=0)=zad6C2POM(c)(xs)(licznik+1)
	|(c==x && length (xs)==0)=licznik+1
	|(c/=x && length (xs)/=0)=zad6C2POM(c)(xs)(licznik)
	|otherwise=licznik


-- zadnie 7
zadanie7 :: a -> Int -> [a]
zadanie7 _ 0 = []
zadanie7 b n = replicate n b

-- zadnie 8
zadanie8 x = (x == reverse x)

-- zadnie 9
zadanie9 p [] = []
zadanie9 p (x:xs) 
	| p == x = xs
	| otherwise = x : zadanie9 p xs


-- zadnie 10
zadanie10 x i = take (i-1) x ++ drop (i) x

-- zadnie 11
zadanie11 a b
	|(all (`elem` a) b)&&(all (`elem` b) a)=True
	|otherwise=False

-- zadnie 12
--zadanie12:: [] -> []
zadanie12 [] =[]
zadanie12 (x:xs) = [(snd x,fst x)]++ zadanie12 xs



	





