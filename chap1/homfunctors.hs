{- defined in default startup environment
class Functor f where
  fmap :: (a -> b) -> f a - f b

instance Functor ((->) r) where
  fmap = (.)

instance Functor ((,) a) where
  fmap f (x,y) = (x, f y)
-}

class Contra f where
  pamf ::  (a -> b) -> f b -> f a

newtype Moh b a = Moh {getHom :: a -> b}

instance Contra (Moh b) where
  pamf f (Moh g) = Moh (g . f)

newtype Riap b a = Riap {getPair :: (a,b)}

instance Functor (Riap b) where
  fmap f (Riap (x,y)) =  Riap (f x,y)

{- suggested tests
fmap (\x -> x*x) (\x -> x + 1) 10 == 121
getHom (pamf (\x -> x*x) (Moh (\x -> x + 1))) 10 == 101

fmap (\x -> x*x) (10,10) == (10,100)
getPair (fmap (\x -> x*x) (Riap (10,10))) == (100,10)
-}