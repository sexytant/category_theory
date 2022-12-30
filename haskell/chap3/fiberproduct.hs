data A0 = Va | Vb  deriving (Eq, Show)

data A1 = V1 | V2 | V3 | V4 deriving (Eq, Enum, Show)

data A2 = V5 | V6 | V7 | V8 deriving (Eq, Enum, Show)

fiberProduct :: (Eq a, Eq b, Eq c) => 
                (b -> a) -> (c -> a) -> [b] -> [c] -> [(a,b,c)]
fiberProduct p q xs ys =
  [(p x,x,y) | x <- xs, y <- ys, p x == q y]

getFiber :: Eq a => a -> [(a,b,c)] -> [(b,c)]
getFiber bp zxys = [(x,y) | (z,x,y)<- zxys, z == bp]

-- testdata
a1s = [V1 .. V4]
a2s = [V5 .. V8]

a1 :: A1 -> A0
a1 V1 = Va
a1 V2 = Va
a1 V3 = Vb
a1 V4 = Vb

a2 :: A2 -> A0
a2 V5 = Va
a2 V6 = Vb
a2 V7 = Va
a2 V8 = Vb

{-- suggested tests
fiberProduct a1 a2 a1s a2s
getFiber Va $ fiberProduct a1 a2 a1s a2s
getFiber Vb $ fiberProduct a1 a2 a1s a2s
-}