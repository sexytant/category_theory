data A0 = Va | Vb  deriving (Eq, Show)

data A1 = V1 | V2 | V3 | V4 deriving (Eq, Enum, Show)

data A2 = V5 | V6 | V7 | V8 deriving (Eq, Enum, Show)

-- Disjoint Union with Gluing
data DUwG a b c =
  L b | R c | G a b c  deriving (Eq, Show)

fibercoproduct :: (Eq a, Eq b, Eq c) => 
  (a -> b) -> (a -> c) -> [a] -> [b] -> [c] -> [DUwG a b c]
fibercoproduct p q as xs ys =
  [L x | x <- xs, not (x `elem` (fmap p as)) ] ++
  [G a x y | 
   x <- xs, y <- ys, a <- as, x == p a, y == q a] ++
  [R y | y <- ys, not (y `elem` (fmap q as)) ]


-- testdata
a0s = [Va, Vb]
a1s = [V1 .. V4]
a2s = [V5 .. V8]

a1 :: A0 -> A1
a1 Va = V1
a1 Vb = V2

a2 :: A0 -> A2
a2 Va = V6
a2 Vb = V8

{-- suggested tests
fibercoproduct a1 a2 a0s a1s a2s
-}
