{-
----------------------------------------- zad4 -----------------------------------------
  Napisać funkcje zwracające:
    a) drugi element listy,
    b) trzeci element listy,
    c) przedostatni element listy
-}

----------------------------------------- Rozwiązanie -----------------------------------------
-- Napisac funkcje zwracającą:
-- a) drugi element listy
-- TODO: sprobowac z Nothing - Monad Haskell

zwrocDrugi :: [Int] -> Int
zwrocDrugi l = if length l > 1 then head (tail l) else -1

-- b) trzeci element listy
zwrocTrzeci :: [Int] -> Int
zwrocTrzeci [] = -1
zwrocTrzeci (x: []) = -1
zwrocTrzeci (x:y:[]) = -1
zwrocTrzeci (x:y:z:_) = z

zwrocTrzeci2 :: [Int] -> Int
zwrocTrzeci2 l = if length l < 3 then -1 else zwrocTrzeciPomoc l
zwrocTrzeciPomoc (x:y:z:_) = z

--c) przedostatni element listy
zwrocPrzedostatni l = if length l > 1 then last (init l) else -1
-- init zwraca cala liste bez ostatniego lementu, last zwraca ostatni elment listy
-- http://learnyouahaskell.com/starting-out#an-intro-to-lists rysunek z robakiem
