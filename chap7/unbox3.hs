{-# LANGUAGE RankNTypes #-}

factory3 :: (c -> a) -> (forall b . (a -> b) -> (c -> b))
factory3 a f = f . a

unbox3 :: (forall b . (a -> b) -> (c -> b)) -> (c -> a)
unbox3 t = t id

-- test data
machine3 = factory3 (\x -> x + 1)

{- tests
map (machine3 (\x -> x*x)) [1..10]

map (unbox3 machine3) [1..10]
-}
