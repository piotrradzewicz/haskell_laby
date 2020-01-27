------------------------------ zad3 ------------------------------

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show-- typ rekurencyjny
-- a) drzewo Intów
t1 :: Tree Int -- drzewo przechujace inty
t1 = Node 1
      ( Node 2 -- lewy II
          (Node 4 -- lewy III
            Empty
            Empty
          )
          (Node 5 -- prawy III
            Empty
            (Node 8 -- prawy IV
              Empty
              Empty
            )
          )
      )
      ( Node 3 -- prawe II
          (Node 6 -- lewe III
            Empty
            (Node 9 -- prawe IV
              Empty
              Empty
            )
          )
          (Node 7 -- prawe III
            Empty
            Empty
          )
      )


-- b) drzewo z char
t2 :: Tree Char -- drzewo przechujace inty
t2 = Node 'a'
      (Node 'b' -- lewe II
        Empty
        (Node 'd' -- prawe III
          (Node 'f' Empty Empty) -- lewe IV
          Empty
        )
      )
      (Node 'c' -- prawe II
        (Node 'e' -- lewe III
          Empty
          (Node 'g' Empty Empty) -- prawe IV
        )
        Empty

      )

-- Sprawdzić działanie funkcji preorder
-- preorder - najpierw zostaje odwiedzony wierzchołk a potem jego poddrzewa
preorder :: Tree a -> [a]
preorder Empty = [] -- pusta lista dla pusteo drzewa
preorder (Node a nodeL nodeR) = [a] ++ preorder nodeL ++ preorder nodeR
            -- aktualna wartosc wierzchołka ++ wartosc z lewego wierzchołka ++ wartosc z prawego wierzchołka
-- *Main> preorder t1
-- [1,2,4,5,8,3,6,9,7]
-- *Main> preorder t2
-- "abdfceg"

-- Sprawdzić działanie funkcji postorder
-- postorder - wierzchołek drzewa zostaje odwiedzony po odwiedzeniu jego lewego i prawego poddrzewa
-- najpiew wchodzę głębiej w drzewo
postorder :: Tree a -> [a]
postorder Empty = [] -- pusta lista dla pustego drzewa
postorder (Node a nodeL nodeR) = postorder nodeL ++ postorder nodeR ++ [a] -- na koncu dodaje element z wierzchołka
-- *Main> postorder t1
-- [4,8,5,2,9,6,7,3,1]
-- *Main> postorder t2
-- "fdbgeca"

-- Sprawdzić działanie funkcji inorder
-- inorder - wierzchołek zostaje odwiedzony po odwiedzeniu lewego i przed odwiedzeniem prawego poddrzewa
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a nodeL nodeR) = inorder nodeL ++ [a] ++ inorder nodeR
