import Coequalizer

-- Category J
---- Obj(J) = {1, 2}
data ObjJ = O1 | O2  deriving (Eq, Enum, Show)

---- Mor(J) = {1_1, 1_2, f}
data MorJ = I1 | I2 | Mf  deriving (Eq, Enum, Show)

---- dom : Mor(J) -> Obj(J)
domJ :: MorJ -> ObjJ
domJ I1 = O1
domJ I2 = O2
domJ Mf = O1

---- cod : Mor(J) -> Obj(J)
codJ :: MorJ -> ObjJ
codJ I1 = O1
codJ I2 = O2
codJ Mf = O2

-- toy sets
---- F(1) = {3,4,5,6}
data F1 = V3 | V4 | V5 | V6  deriving (Eq, Show)

---- F(2) = {7,8,9}
data F2 = V7 | V8 | V9  deriving (Eq, Show)

---- F(1_1)
fmapI1 :: F1 -> F1
fmapI1 x = x

---- F(1_2)
fmapI2 :: F2 -> F2
fmapI2 x = x

---- F(f)
fmapMf :: F1 -> F2
fmapMf V3 = V7
fmapMf V4 = V7
fmapMf V5 = V9
fmapMf V6 = V9

-- F(1) + F(2) + F(1) -- disjoint union
data F121 = P3 | P4 | P5 | P6
            | Q7 | Q8 | Q9
            | R3 | R4 | R5 | R6
  deriving (Eq, Enum, Show)

-- coprojections to F(1)+F(2)+F(1)
sigma1 :: F1 -> F121
sigma1 V3 = P3
sigma1 V4 = P4
sigma1 V5 = P5
sigma1 V6 = P6

sigma2 :: F2 -> F121
sigma2 V7 = Q7
sigma2 V8 = Q8
sigma2 V9 = Q9

sigma'f :: F1 -> F121
sigma'f V3 = R3
sigma'f V4 = R4
sigma'f V5 = R5
sigma'f V6 = R6

-- partial inverses to coprojections
sigma1i :: F121 -> F1
sigma1i P3 = V3
sigma1i P4 = V4
sigma1i P5 = V5
sigma1i P6 = V6

sigma2i :: F121 -> F2
sigma2i Q7 = V7
sigma2i Q8 = V8
sigma2i Q9 = V9

sigma'fi :: F121 -> F1
sigma'fi R3 = V3
sigma'fi R4 = V4
sigma'fi R5 = V5
sigma'fi R6 = V6

-- F(1) + F(2) -- disjoint union
data F12  = S3 | S4 | S5 | S6 | T7 | T8 | T9
  deriving (Eq, Enum, Show)

-- coprojections to F(1)+F(2)
rho1 :: F1 -> F12
rho1 V3 = S3
rho1 V4 = S4
rho1 V5 = S5
rho1 V6 = S6

rho2 :: F2 -> F12
rho2 V7 = T7
rho2 V8 = T8
rho2 V9 = T9

-- t: F(1)+F(2)+F(1) -> F(1)+F(2)
t :: F121 -> F12
t x
  | x `elem` [P3 .. P6]  = rho1 (sigma1i x)
  | x `elem` [Q7 .. Q9]  = rho2 (sigma2i x)
  | x `elem` [R3 .. R6]  = rho1 (sigma'fi x)

{- suggested test for t
map t [P3 .. R6]
-}

-- s: F(1)+F(2)+F(1) -> F(1)+F(2)
s :: F121 -> F12
s x
  | x `elem` [P3 .. P6]  = rho1 (fmapI1 (sigma1i x))
  | x `elem` [Q7 .. Q9]  = rho2 (fmapI2 (sigma2i x))
  | x `elem` [R3 .. R6]  = rho2 (fmapMf (sigma'fi x))

{- suggested test for s
map s [P3 .. R6]
-}

{- do it
rel = coequate s t [P3 .. R6]

partition rel [S3 .. T9]
-}