import Tree
import Flatten

iSort :: Ord a => [a] -> [a]
iSort xs =
  foldr ins [] xs
    where
      ins x [] = [x]
      ins x (y:ys)
        | x <= y    = x:y:ys
        | otherwise = y: ins x ys

list2tree :: Ord a => [a] -> Tree a
list2tree xs =
  foldl sni Empty xs
    where
      sni = flip ins
      ins x Empty   = Node x Empty Empty
      ins x (Node y t1 t2)
        | x <= y    = Node y (ins x t1) t2
        | otherwise = Node y t1 (ins x t2)

iSort2 :: Ord a => [a] -> [a]
iSort2 = flatten . list2tree

{- suggested tests
map (\x -> x*2) . iSort $ [1,5,3,4,2]
iSort . map (\x -> x*2) $ [1,5,3,4,2]
-}
