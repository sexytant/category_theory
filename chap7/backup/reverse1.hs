{-# LANGUAGE RankNTypes #-}

check1 :: a -> (forall b . (a -> b) -> b)
check1 a f = f a

uncheck1 :: (forall b . (a -> b) -> b) -> a
uncheck1 t = t id

-- testdata
machine1 = check1 10

{- suggested tests
machine1 (\x -> x*x)
uncheck1 machine1
-}

{-
data I a = I {unI :: a} deriving Show
instance Functor I where
  fmap f (I a) = I (f a)

check1 :: a -> (forall b . (a -> b) -> I b)
check1 a f = I (f a)

uncheck1 :: (forall b . (a -> b) -> I b) -> a
uncheck1 t = unI (t id)
-}