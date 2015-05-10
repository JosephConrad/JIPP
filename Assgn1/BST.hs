-- Konrad Lisiecki 291649
-- Implementacja zrownowazonego drzewa BST przy pomocy drzew cz-cz
module BST where

data Tree a = Empty | Node { key :: Color, val :: a, left :: Tree a,  right:: Tree a } deriving (Eq, Show)
data Color = Red | Black deriving (Eq,Show)

empty :: Tree a
empty = Empty

depth :: Tree a -> Int
depth Empty = 0
depth (Node _ a left right) = max (depth $ left) (depth $ right) + 1

toList :: Tree a -> [a]
toList Empty = []
toList (Node _ a left right) = (toList left) ++ (a:(toList right))

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Empty
fromList x = fromList1 x Empty

fromList1 :: (Ord a) => [a] -> Tree a -> Tree a
fromList1 [] t = t
fromList1 (x:xs) t = fromList1 xs (insert x t) 

insert :: Ord a => a -> Tree a -> Tree a
insert a t = repaint $ update a t 

-- Aby wlozyc element do drzewa cz-cz nalezy:
--
-- Wlozyc element jak do drzewa BST
-- Kolor tego elementu powinien byc czerowny
-- Gdy bedzie zachwiana wlasnosc drzewa cz-cz, przywrocic ja

update :: Ord a => a -> Tree a -> Tree a
update a Empty  = Node Red a Empty Empty 
update a (Node Black val left right)
        | a <= val = rotate val (update a left) right 
        | a > val  = rotate val left (update a right)
update a (Node Red val left right)
        | a <= val = Node Red val (update a left) right 
        | a > val  = Node Red val left (update a right)

repaint :: Ord a => Tree a -> Tree a
repaint t = Node Black (val t) (left t) (right t) 

-- Rotacje dla poszczegolnych przypadkow. Wyrozniamy 2 rodzaje rotacji i w 
--  dwie strony, czyli lacznie mamy 4 przypadki
rotate :: a -> Tree a -> Tree a -> Tree a
-- sciezka  czerwonych wierzcholkow w lewo
rotate v1 (Node Red v2 (Node Red v3 t1 t2) t3) t4 = 
    Node Red v2 (Node Black v3 t1 t2) (Node Black v1 t3 t4)

-- sciezka czerwonych wierzcholow w prawo
rotate v1 t1 (Node Red v2 t2 (Node Red v3 t3 t4)) =
     Node Red v2 (Node Black v1 t1 t2) (Node Black v3 t3 t4)

-- sciezka czerwonych w lewo i prawo 
rotate v1 (Node Red v2 t1 (Node Red v3 t2 t3)) t4 = 
    Node Red v3 (Node Black v2 t1 t2) (Node Black v1 t3 t4)

-- sciezka czerwonych w prawo i lewo  
rotate v1 t1 (Node Red v2 (Node Red v3 t2 t3) t4) = 
    Node Red v3 (Node Black v1 t1 t2) (Node Black v2 t3 t4)

rotate v t1 t2 = Node Black v t1 t2 
