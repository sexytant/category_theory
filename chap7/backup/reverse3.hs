{-# LANGUAGE RankNTypes #-}

check3 :: (c -> a) -> (forall b . (a -> b) -> (c -> b))
check3 a f = f . a

uncheck3 :: (forall b . (a -> b) -> (c -> b)) -> (c -> a)
uncheck3 t = t id

-- test data
machine3 = check3 (\x -> x + 1)

{- tests
map (machine3 (\x -> x*x)) [1..10]

map (uncheck3 machine3) [1..10]
-}
