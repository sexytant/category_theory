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
---- F(1) = {3,4,5}
data F1 = V3 | V4 | V5  deriving (Eq, Show)

---- F(2) = {6,7}
data F2 = V6 | V7 deriving (Eq, Show)

---- F(1_1)
fmapI1 :: F1 -> F1
fmapI1 x = x

---- F(1_2)
fmapI2 :: F2 -> F2
fmapI2 x = x

---- F(f)
fmapMf :: F1 -> F2
fmapMf V3 = V6
fmapMf V4 = V6
fmapMf V5 = V7

-- t
t :: (F1,F2) -> (F1,F2,F2)
t (x,y) = (x,y,y)

-- s
s :: (F1,F2) -> (F1,F2,F2)
s (x,y) = (fmapI1 x, fmapI2 y, fmapMf x)

-- equalizer
objK = [(x,y) | x <- [V3,V4,V5], y <- [V6,V7],
                s (x,y) == t (x,y) ]