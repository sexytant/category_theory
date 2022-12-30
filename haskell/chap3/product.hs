data SetA = A7 | A8 | A9 deriving (Enum,Show)

data SetB = Ba | Bb | Bc deriving (Enum,Show)

data SetX = X1 | X2 | X3 | X4 deriving (Enum,Show)

x1 :: SetX -> SetA
x1 X1 = A7
x1 X2 = A8
x1 X3 = A9
x1 X4 = A7

x2 :: SetX -> SetB
x2 X1 = Ba
x2 X2 = Bb
x2 X3 = Bc
x2 X4 = Bc

factor :: (x -> a) -> (x -> b) -> (x -> (a,b))
factor p1 p2 x = (p1 x, p2 x)

{- suggested tests
:t factor p1 p2
[factor x1 x2 x | x <- [X1 .. X4] ]
-}