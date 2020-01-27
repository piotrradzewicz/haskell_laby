-- Zdefiniować typ o nazwie "moto" którego konstruktorami sa nazwy marek samochodow
 -- (5 różnych marek).Napisz definicje
-- data Car = Car {marka:: String , kraj:: String, predkoscSrednia :: Float } deriving Show -- nie moge w dziedziczenie

-- konstruktor bezargumentowy
data Moto = BMW  | Audi | Dacia | Fiat | Citroen deriving (Eq, Ord, Show)

 -- a) funkcji, która nazwie państawa przypisuje jedną markę samochodu
 -- produkowanego w tym państwie, dla państwa zdefiniowac synonim typu [Cahr] o nazwie Kraj
type Kraj = [Char] --synonim typu

panstwoMarka :: Kraj -> Moto
panstwoMarka k
  | k == "PL" = BMW
  | k == "GB" = Audi
  | k == "UE" = Dacia
  | k == "BR" = Fiat
  | k == "HR" = Citroen
  | otherwise = error "Brak samochodu dla tego państwa"


-- b) funkcji, która danej marce samochodu przypisuje średnia predkosc jaką mozeosiągnać aut tej marki
-- sredniaPredkosc :: Moto -> Float
-- sredniaPredkosc moto predkosc = BMW k predkosc -- typy są immutable - wiec zwracam nowy obiekt tego typu i go przypisuje
sredniaPredkosc :: Moto -> Float
sredniaPredkosc m
  | m == BMW = 1.1
  | m == Audi = 2.2
  | m == Dacia = 3.3
  | m == Fiat = 4.4
  | m == Citroen = 5.5
  | otherwise = error "Brak takiej marki samochodu" -- dla braku marki samochodu  Data constructor not in scope: Feee :: Moto
