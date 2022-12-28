{-# LANGUAGE RankNTypes #-}

data I a = I a deriving Show
instance Functor I where
  fmap f (I a) = I (f a)

{-  Defined in ‘GHC.Base'
instance Functor ((->) a) where
  fmap f = (.) f
-}

check :: Functor f => f a -> (forall b . (a -> b) -> f b)
check a f = fmap f a

uncheck :: (forall b . (a -> b) -> f b) -> f a
uncheck t = t id

-- test data
machine41 = check (I 10)
machine42 = check [1,2,3,4]
machine43 = check (\x -> x + 1)

{- tests
map machine41 [(\x -> x*2), (\x -> x*x), (\x -> x*x*x)]
uncheck machine41

machine42 (\x -> x*x)
uncheck machine42

map (machine43 (\x -> x*x)) [1..10]
map (uncheck machine43) [1..10]
-}

