----------------------------------------- zad5 -----------------------------------------
-- Zdefiniować funkcję podnoszącą do kwadratu wszystkie elementy danej listy liczb
-- całkowitych, np. sqrlist [1,2,-3] zwraca [1, 4, 9].

--   a) z użyciem map,
kwadrat l = map (^2) l

-- b) bez uzycia map (list comprehension)
kwadrat2 l = [x^2 | x <-l]
