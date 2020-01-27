------------------------------ zad11 ------------------------------
-- Napisać definicję dwuargumentowej funkcji określonej dla list, której wartośc
 -- jest prawda jesli wszystkie elementy pierwszej listy wystepuja w rugiej liscie

spr:: Eq a => [a] -> [a] -> Bool
spr [] l2 = True
spr l1 [] = False
spr (x:xs) l2 = if x `elem` l2 then spr xs l2 else False
