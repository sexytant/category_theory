data XA = A1 | A2 | A3
  deriving (Show, Eq, Enum)

data XB = B1 | B2
  deriving (Show, Eq, Enum)

data XC = C1 | C2 | C3 | C4 | C5 | C6
  deriving (Show, Eq, Enum)

-- Hom(XC, (XA times XB)) ~= Hom(XC, XA) times Hom(XC, XB)
alpha1 :: (c -> (a,b)) -> (c -> a, c -> b)
alpha1 f
  = (fst . f, snd . f)

alpha2 :: (c -> a, c -> b) -> (c -> (a,b))
alpha2 (f,g)
  = \c -> (f c, g c)

-- Hom(XA + XB, XC) ~= Hom(XA, XC) times Hom(XB, XC)
beta1 :: (Either a b -> c) -> (a -> c, b -> c)
beta1 f
  = (f . Left, f . Right)

beta2 :: (a -> c, b -> c) -> (Either a b -> c)
beta2 (f,g)
  = h
     where
       h (Left a)  = f a
       h (Right b) = g b

-- test data for
-- alpha1 . alpha2 = id    and 
-- alpha2 . alpha1 = id

f1 :: XC -> (XA,XB)
f1 C1 = (A1,B1)
f1 C2 = (A1,B2)
f1 C3 = (A2,B1)
f1 C4 = (A2,B2)
f1 C5 = (A3,B1)
f1 C6 = (A3,B2)

f2 :: XC -> XA
f2 C1 = A1
f2 C2 = A1
f2 C3 = A2
f2 C4 = A2
f2 C5 = A3
f2 C6 = A3

g2 :: XC -> XB
g2 C1 = B1
g2 C2 = B1
g2 C3 = B1
g2 C4 = B2
g2 C5 = B2
g2 C6 = B2

{- executable tests
map ((alpha2 . alpha1) f1) [C1 .. C6]
map f1 [C1 .. C6]

map (fst ((alpha1 . alpha2) (f2,g2)) ) [C1 .. C6]
map f2 [C1 .. C6]
map (snd ((alpha1 . alpha2) (f2,g2)) ) [C1 .. C6]
map g2 [C1 .. C6]
-}

-- test data for
-- beta1 . beta2 = id    and 
-- beta2 . beta1 = id

f3 :: Either XA XB -> XC
f3 (Left A1)  = C1
f3 (Left A2)  = C2
f3 (Left A3)  = C3
f3 (Right B1) = C4
f3 (Right B2) = C5

d1 :: [Either XA XB]
d1 = (map Left [A1 ..]) ++ (map Right [B1 ..])

f4 :: XA -> XC
f4 A1 = C1
f4 A2 = C2
f4 A3 = C3

g4 :: XB -> XC
g4 B1 = C4
g4 B2 = C5

{- executable tests
map ((beta2 . beta1) f3) d1
map f3 d1

map (fst ((beta1 . beta2) (f4,g4))) [A1 ..]
map f4 [A1 ..]

map (snd ((beta1 . beta2) (f4,g4))) [B1 ..]
map g4 [B1 ..]
-}