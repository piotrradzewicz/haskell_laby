import Control.Monad (unless)
--zad1
zad1 = do putStrLn ("Podaj dwie liczby")
          n<-getLine
          m<-getLine
          let x = read n
          let y = read m
              a=x+y
              b=x*y
              c=x-y
          putStrLn("suma: " ++ show a ++ " iloczyn: " ++ show b ++ " roznica: " ++ show c)

--zad2
zad2 = do putStrLn ("Podaj dwie liczby")
          n<-getLine
          m<-getLine
          let x = read n
          let y = read m
              a=my_gcd y (x `mod` y)
              b= abs (x `quot` (gcd x y) * y)
          putStrLn("nwd: " ++ show a ++" nww: " ++ show b)


my_gcd :: Int -> Int -> Int
my_gcd a b
	| b == 0    = a
	| otherwise = my_gcd b (a `mod` b)

my_lcm :: Int -> Int -> Int
my_lcm a b = abs (a `quot` (gcd a b) * b)



--zad3
zad3 = do putStrLn ("Podaj swoje imie i nazwisko")
          (n:ns)<-getLine
          (m:ms)<-getLine
          putStrLn("Twoje inicjały to: " ++ show n ++ show m)

--zad4

zad4 = do putStrLn ("Podaj liczbe od 0 do 99")
          n<-getLine
          	if (n==9) then "ddd"  else "sss"
          putStrLn("nwd: ")

 
for (x:xs) f = do
  f x
  unless (null xs) $ for xs f
 
main2 = for [1..10] (\i -> putStrLn ("Hello: " ++ show i))

spr a 
	|a<40="Mniejsza"
	|otherwise="Zgadleś"
