import Tree

mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Node x t1 t2) = Node x (mirror t2) (mirror t1)

{- compare the following two
-- in Tree.hs, we gave no instance of Eq for Tree
mirror . mirror $ tree1
tree1
-}

{- suggested tests
mirror . fmap string2int $ tree1
fmap string2int . mirror $ tree1
-}