-- Konrad Lisiecki 291649

module TreeDict (Dict, empty, insert, lookup, fromList, toList, fastInsert) where 

import Prelude hiding(lookup)

data Dict k v = Empty | Node { key :: k, val :: v, left :: Dict k v, right :: Dict k v }
    deriving (Eq, Show)
 
empty :: Dict k v
empty = Empty

insert :: (Ord k) => k -> v -> Dict k v -> Dict k v
insert k v Empty = Node k v empty empty
insert k v d  
        | k < key d = Node (key d) (val d) (insert k v (left d)) (right d)
        | k > key d = Node (key d) (val d) (left d) (insert k v (right d))
        | otherwise = Node k v (left d) (right d)

lookup :: (Ord k) => k -> Dict k v -> Maybe v
lookup k Empty = Nothing
lookup k d 
        | k < key d = lookup k (left d)
        | k > key d = lookup k (right d)
        | otherwise = Just (val d)

fromList :: Ord k => [(k, v)] -> Dict k v
fromList [] = Empty
fromList x = fromList1 (reverse x) empty 

fromList1 :: Ord k => [(k, v)] -> Dict k v -> Dict k v
fromList1 [] t = t
fromList1 ((k,v):xs) t = fromList1 xs (insert k v t)

toList :: Dict k v -> [(k, v)]
toList Empty = []
toList (Node key val left right) = (toList left) ++ ((key, val):(toList right)) 

fastInsert :: (Ord k) => k -> (Maybe v -> v) -> Dict k v ->  Dict k v
fastInsert k update Empty = Node k (update Nothing) empty empty
fastInsert k update d  
        | k < key d = Node (key d) (val d) (fastInsert k update (left d)) (right d)
        | k > key d = Node (key d) (val d) (left d) (fastInsert k update (right d))
        | otherwise = Node k (update (Just (val d))) (left d) (right d)