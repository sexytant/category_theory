data A1 = V1 | V2 | V3 | V4 
  deriving (Eq, Enum, Show)

data A2 = Va | Vb | Vc  deriving (Eq, Enum, Show)

-- We will be content with 
-- finding a subobject(subset) of A1
equalizer :: (Eq a, Enum a, Eq b) => 
  (a -> b) -> (a -> b) -> [a] -> [a]
equalizer f g xs
  = [x | x <- xs, f x == g x]

f :: A1 -> A2
f V1 = Va
f V2 = Vb
f V3 = Vc
f V4 = Vc

g :: A1 -> A2
g V1 = Vb
g V2 = Vb
g V3 = Vc
g V4 = Va

{-- suggested tests
equalizer f g [V1 .. V4]
-}