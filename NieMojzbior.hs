module Mojzbior (podzbior, iloczyn, suma, roznica) where


podzbiora x [] = False
podzbiora x (y:ys)
 | x == y = True
 | otherwise = podzbiora x ys
 
podzbiorb (x:xs) (y:ys) = [if podzbiora x (y:ys) == False then 1 else 0|x<-(x:xs)] 

podzbior [] [] = True
podzbior [] (y:ys) = True
podzbior (x:xs) (y:ys)
 | sum (podzbiorb (x:xs) (y:ys)) == 0 = True
 | otherwise = False


 
iloczyn [] _ = []
iloczyn _ [] = [] 
iloczyn (x:xs) (y:ys)
 | elem x (y:ys) = x : (iloczyn (xs) (y:ys))
 | otherwise = iloczyn (xs) (y:ys)


sumacz (x:xs) (y:ys) = [y|y<-(y:ys), podzbiora y (x:xs) == False] 
suma (x:xs) (y:ys) = (x:xs) ++ (sumacz (x:xs) (y:ys))

roznica (x:xs) (y:ys) = [x|x<-(x:xs), podzbiora x (y:ys) == False] 