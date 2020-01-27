------------------------------ zad2 ------------------------------
--  Napisz funkcje przestawiajaća w liscie ostatni element z pierwszym
przestaw :: [Int] -> [Int]
przestaw l = if length l >= 2 then ostatni ++ srodek ++ pierwszy else []
          where pierwszy = [head l]
                ostatni = [last l]
                srodek = init (tail l)
