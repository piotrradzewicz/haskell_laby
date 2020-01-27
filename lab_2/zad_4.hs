------------------------------ zad4 ------------------------------
-- Napisac funkcje sprawdzajaca czy lista ma parzysta liczbe elementow
czyDlugoscParzysta :: [Int] -> Bool
czyDlugoscParzysta l = if length l `mod` 2 == 0 then True else False
