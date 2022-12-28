import Tree
-- Walk tree
numberTree :: Tree a -> Tree Int
numberTree t =  snd (nTree 1 t)

nTree :: Int -> Tree a -> (Int, Tree Int)
nTree n Empty = (n, Empty)
nTree n (Node x t1 t2) =
  let (n1, it1) = nTree (n+1) t1
      (n2, it2) = nTree n1 t2
  in  (n2, Node n it1 it2)

-- test data
tree2 = Node "two" (Node "three" Empty Empty) 
                   (Node "four" Empty Empty)
tree3 = Node "five" (Node "six" Empty Empty)
                    (Node "seven" Empty Empty)
tree1 = Node "one" tree2 tree3

