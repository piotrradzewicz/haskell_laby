------------------------------ zad2 ------------------------------
-- Zdefiniować typ o nazwie "moto" którego konstruktorami sa nazwy marek samochodow
 -- (5 różnych marek).Napisz definicje
-- data Car = Car {marka:: String , kraj:: String, predkoscSrednia :: Float } deriving Show -- nie moge w dziedziczenie

-- TODO: werjsa z konstruktorem argumentowym;
-- konstruktor bezargumentowy
type SredniaPredkosc = Float
data Car = Car SredniaPredkosc deriving (Eq, Ord, Show)
--
-- polimorfizm w haskellu/dziedziczenie
data Moto = BMW Car | Audi Car | Dacia Car | Fiat Car | Citroen Car deriving (Eq, Ord, Show)
-- no to mogą być problemy What you can do is hide the constructor, and provide a function as constructor instead.
-- https://stackoverflow.com/questions/47339283/default-values-in-haskell-data-types

 -- a) funkcji, która nazwie państawa przypisuje jedną markę samochodu
 -- produkowanego w tym państwie, dla państwa zdefiniowac synonim typu [Cahr] o nazwie Kraj
type Kraj = [Char] --synonim typu

-- b) funkcji, która danej marce samochodu przypisuje średnia predkosc jaką mozeosiągnać aut tej marki
-- sredniaPredkosc :: Moto -> Float
-- sredniaPredkosc moto predkosc = BMW k predkosc -- typy są immutable - wiec zwracam nowy obiekt tego typu i go przypisuje
