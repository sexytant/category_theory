{-# LANGUAGE RankNTypes #-}

data I a = I a deriving Show
instance Functor I where
  fmap f (I a) = I (f a)

{-  Defined in ‘GHC.Base'
instance Functor ((->) a) where
  fmap f = (.) f
-}

factory :: Functor f => f a -> (forall b . (a -> b) -> f b)
factory a f = fmap f a

unbox :: (forall b . (a -> b) -> f b) -> f a
unbox t = t id

-- test data
machine41 = factory (I 10)
machine42 = factory [1,2,3,4]
machine43 = factory (\x -> x + 1)

{- tests
map machine41 [(\x -> x*2), (\x -> x*x), (\x -> x*x*x)]
unbox machine41

machine42 (\x -> x*x)
unbox machine42

map (machine43 (\x -> x*x)) [1..10]
map (unbox machine43) [1..10]
-}

