{-Zadanie 1.
Napisać program, który będzie pytał o dwie liczby, a następnie wypisze ich sumę, iloczyn
i różnicę.-}

c5z1 = do
 putStrLn "podaj pierwsza liczbe: "
 input1 <- getLine
 putStrLn "podaj drugą liczbe: " 
 input2 <- getLine 
 let x = (read input1 :: Int)
 let y = (read input2 :: Int)
 let suma = x + y
 let iloczyn = x * y
 let roznica = x - y
 print (x + y)
 putStrLn ("Suma:" ++ (show suma))
 putStrLn ("Iloczyn:" ++ (show iloczyn))
 putStrLn ("Roznica:" ++ (show roznica))

nwd x 0 = abs x
nwd x y = nwd b (mod a b)
 where a = abs x
       b = abs y

nww _ 0 = 0
nww 0 _ = 0
nww x y = 
 let mult = (x*y) 
     nwdd = nwd x y
 in mult `div` nwdd

{-﻿Zadanie 2.
Napisać program, który będzie pytał o dwie liczby, a następnie wypisze ich największy
wspólny dzielnik i najmniejszą wspólną wielokrotność.-}
 
c5z2 = do
 putStrLn "podaj pierwsza liczbe: "
 input1 <- getLine
 putStrLn "podaj drugą liczbe: " 
 input2 <- getLine 
 let x = (read input1 :: Int)
 let y = (read input2 :: Int)
 let z = nwd x y
 let c = nww x y
 putStrLn ("Największy wspólny dzielnik:" ++ (show z))
 putStrLn ("Najmniejsza wspólna wielokrotnosc:" ++ (show c))

{-﻿Zadanie 3.
Napisać program, który będzie pytał o imię i nazwisko, a następnie wypisze inicjały imienia
i nazwiska.-}

c5z3 = do 
 putStrLn "Imię: "
 input1 <- getLine
 putStrLn "Nazwisko: " 
 input2 <- getLine 
 let x = head input1
 let y = head input2
 putStrLn ((show x)++"."++(show y)++".")


{-Zadanie 4.
Napisać program „grę”, który prosi użytkownika o podanie liczby z zakresu 0-99
i podpowiada, czy wprowadzona liczba jest większa, czy mniejsza od ustalonej liczby.
Program kończy działanie w przypadku odgadnięcia liczby lub po 10 próbach.-}

c5z4 = c5z4a 10

c5z4a 0 = putStrLn ("koniec nie zdadłeś")
c5z4a n =
 do 
  let x = 60
  putStrLn "podaj liczbe od 0-99: "
  input1 <- getLine
  let y = (read input1 :: Int)
  if y == x
   then putStrLn ("zgadłeś " ++ (show x))
   else if y < x 
    then do
     putStrLn ("Większe")
     c5z4a (n-1)
    else do 
     putStrLn ("Mniejsze")
     c5z4a (n-1)


