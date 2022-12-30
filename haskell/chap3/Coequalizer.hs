module Coequalizer(Relation,coequate,collect,partition) where

type Relation b = b -> b -> Bool

-- returns the base relation which is already
-- reflexive and symmetric, but not transitive
coequate :: (Eq a, Eq b) =>
  (a -> b) -> (a -> b) -> [a] -> Relation b
coequate p q xs y1 y2
  | y1 == y2  
    = True
  | otherwise
    = [x | x <- xs, (y1==p x && y2==q x) ||
                    (y2==p x && y1==q x)] /= []

-- returns the equivalence class of an element
collect :: Eq b => Relation b -> b -> [b] -> [b]
collect r x [] = []
collect r x xs =
  iter r [x] []
    where
      iter r ys prev'ys
        | ys == prev'ys   = ys
        | otherwise
            = iter r (onepass r ys xs) ys


-- onepass filtering of equivalent elements
-- Several passes (possibly infinite) are needed
-- to get a complete list of
-- equivalent elements.
onepass :: Eq b => Relation b -> [b] -> [b] -> [b]
onepass r xs xs' =
  foldl binop xs xs'
    where
      binop acc z
        | elem z acc
          = acc
        | [y | y <- acc, r y z] == []
          = acc
        | otherwise
          = z:acc


partition :: Eq b => Relation b -> [b] -> [[b]]
partition r ys =
  foldl binop [] ys
    where
      binop acc y
        | [xs | xs <- acc, elem y xs] /= []
          = acc
        | otherwise
          = collect r y ys : acc


-- test data
data A1 = V1 | V2 | V3 | V4 | V5
  deriving (Eq, Enum, Show)

data A2 = Va | Vb | Vc | Vd | Ve
  deriving (Eq, Enum, Ord, Show)

f :: A1 -> A2
f V1 = Va
f V2 = Vb
f V3 = Vc
f V4 = Vc
f V5 = Ve

g :: A1 -> A2
g V1 = Vb
g V2 = Vb
g V3 = Vc
g V4 = Vd
g V5 = Ve

h :: A1 -> A2
h V1 = Vb
h V2 = Vb
h V3 = Vc
h V4 = Vd
h V5 = Vd

-- test data
rel1 = coequate f g [V1 .. V5]
rel2 = coequate f h [V1 .. V5]

{- test for coequate
:{
[(y1,y2) | y1 <- [Va .. Ve], 
           y2 <- [Va .. Ve], rel1 y1 y2]
:}
:{
rel2result = [(y1,y2) | y1 <- [Va .. Ve], 
                        y2 <- [Va .. Ve],
                        rel2 y1 y2]
:}
filter (\(x,y)-> x<y) rel2result
-}
{- tests for collect
collect rel1 Va [Va .. Ve]
collect rel1 Vc [Va .. Ve]
collect rel1 Ve [Va .. Ve]
collect rel2 Va [Va .. Ve]
collect rel2 Vc [Va .. Ve]
collect rel2 Ve [Va .. Ve]
-}

{- overall test
partition rel1 [Va .. Ve]
-- should give [[Ve],[Vd,Vc],[Vb,Va]]
partition rel2 [Va .. Ve]
-- should give [[Ve,Vd,Vc],[Vb,Va]]
-}
