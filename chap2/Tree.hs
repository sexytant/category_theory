module Tree where
import Data.Char

data Tree a = Empty | Node a (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
  show x = show1 0 x

show1 :: Show a => Int -> (Tree a) -> String
show1 n Empty = ""
show1 n (Node x t1 t2) =
  show1 (n+1) t2 ++
  indent n ++ show x ++ "\n" ++
  show1 (n+1) t1

indent :: Int -> String
indent n = replicate (n*4) ' '

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node x t1 t2) =
    Node (f x) (fmap f t1) (fmap f t2)

-- test data
tree2 = Node "two" (Node "three" Empty Empty) 
                   (Node "four" Empty Empty)
tree3 = Node "five" (Node "six" Empty Empty)
                    (Node "seven" Empty Empty)
tree1 = Node "one" tree2 tree3

string2int :: String -> Int
string2int "one"   = 1
string2int "two"   = 2
string2int "three" = 3
string2int "four"  = 4
string2int "five"  = 5
string2int "six"   = 6
string2int "seven" = 7
string2int _       = 0

tree0 = fmap string2int tree1

{- suggested tests
fmap (map toUpper) tree1
fmap string2int tree1
fmap length tree1
-}

{- another possible instance of Show
show1 :: Show a => Int -> (Tree a) -> String
show1 n Empty = indent n ++ "E"
show1 n (Node x t1 t2) =
  indent n ++ show x ++ "\n" ++
  show1 (n+1) t1 ++ "\n" ++
  show1 (n+1) t2
-}

{- original instance
instance (Show a) => Show (Tree a) where
  show x = show1 0 x

show1 :: Show a => Int -> (Tree a) -> String
show1 n Empty = ""
show1 n (Node x t1 t2) =
  indent n ++ show x ++ "\n" ++
  show1 (n+1) t1 ++
  show1 (n+1) t2
-}