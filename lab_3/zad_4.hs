------------------------------ zad4 ------------------------------
-- Zdefiniować funkcję tree-member która sprawdza przynależność elementu do poddrzewa
-- a) z wykorzystaniem funkcji elem
data Tree a = Empty | Node a (Tree a) (Tree a)
t  :: Tree Int
t = Node 1 -- drzewo do testow z poprzedniego
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

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a nodeL nodeR) = inorder nodeL ++ [a] ++ inorder nodeR

isElementTree :: Eq a => a -> Tree a -> Bool -- musi miec Eq z przodu
isElementTree a tree = a `elem` treeList
                      where treeList = inorder tree

-- b) bez wykorzystania funkcji elem
isElementTree' :: Eq a => a -> Tree a -> Bool -- musi miec Eq z przodu
isElementTree' a tree = isListElement a treeList
                      where treeList = inorder tree

isListElement :: Eq a => a -> [a] -> Bool
isListElement _ [] = False
isListElement a (x:xs) = if a == x then True else isListElement a xs


-- tak nie umiem - w razie False ktore poddrzewo jak sprawdzać?
-- isTreeElemnt'' :: Eq a => a -> Tree a -> Bool
-- isTreeElemnt'' e Empty = False
-- isTreeElemnt'' e (Node a nodeL nodeR) = if e == a then True else isTreeElemnt'' nodeL isTreeElemnt'' nodeR
