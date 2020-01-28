module Mojzbior where 
--podzbior:: [a]->[a]->Bool
podzbior [] _ =True
podzbior (x:xs) a = if elem x a==True then podzbior xs  a else False

--iloczyn:: [a]->[a]->[a]
iloczyn [] _ =[]
iloczyn (x:xs) a = iloczynPOM (x:xs) a []
iloczynPOM (x:xs) a wynik

	|(elem x a==True && xs/=[])=  iloczynPOM xs a ([x] ++ wynik) 
	|(elem x a==False && xs/=[])= iloczynPOM xs a wynik
	|(elem x a==True && xs==[])= [x] ++ wynik
	|(elem x a==False && xs==[])= wynik
	|otherwise=[]

--suma:: [a]->[a]->[a]
suma [] a =a
suma a [] =a
suma a b = iloczyn a b ++ roznica a b ++ roznica b a

--roznica:: [a]->[a]->[a]
roznica [] _ =[]
roznica (x:xs) a = roznicaPOM (x:xs) a []
roznicaPOM (x:xs) a wynik

	|(elem x a==False && xs/=[])=  roznicaPOM xs a ([x] ++ wynik) 
	|(elem x a==True && xs/=[])= roznicaPOM xs a wynik
	|(elem x a==False && xs==[])= [x] ++ wynik
	|(elem x a==True && xs==[])= wynik
	|otherwise=[]
--ZAD 4
zippuj [] _ = []
zippuj _ [] = []
zippuj (x:xs) (y:ys) = (x,y):zippuj xs ys
unzippuj [] = ([], [])
unzippuj xs = (map fst xs, map snd xs)



	





