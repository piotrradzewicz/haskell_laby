------------------------------ zad12 ------------------------------
-- Napisac definicję funkcji, której argumentem jest lista krotek 2-elmentowych,
--  a wartością lista krotek z przestawionymi elmentami np. wartścią
--  zamiana [(1, 'a'), (2, 'b')] jest [('a',1), ('b', 2)]
zamiana :: [(b1, b2)] -> [(b2, b1)]
zamiana l = [ (snd x, fst x)|  x <- l]
