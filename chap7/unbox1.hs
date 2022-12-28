{-# LANGUAGE RankNTypes #-}

factory1 :: a -> (forall b . (a -> b) -> b)
factory1 a f = f a

unbox1 :: (forall b . (a -> b) -> b) -> a
unbox1 t = t id

-- testdata
machine1 = factory1 10

{- suggested tests
machine1 (\x -> x*x)
unbox1 machine1
-}

{-
data I a = I {unI :: a} deriving Show
instance Functor I where
  fmap f (I a) = I (f a)

factory1 :: a -> (forall b . (a -> b) -> I b)
factory1 a f = I (f a)

unbox1 :: (forall b . (a -> b) -> I b) -> a
unbox1 t = unI (t id)
-}