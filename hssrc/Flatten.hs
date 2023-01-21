module Flatten where
import Tree
import Data.Char

flatten :: Tree a -> [a]
flatten Empty = []
-- flatten (Node x t1 t2) = x:(flatten t1 ++ flatten t2)
flatten (Node x t1 t2) = flatten t1 ++ [x] ++ flatten t2

toupper = map toUpper

{- suggested tests; compare the results below
flatten . fmap toupper  $ tree1
map toupper . flatten $ tree1
-}