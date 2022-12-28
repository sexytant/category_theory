{-# LANGUAGE RankNTypes #-}

factory2 :: [a] -> (forall b . (a -> b) -> [b])
factory2 a f = map f a

unbox2 :: (forall b . (a -> b) -> [b]) -> [a]
unbox2 t = t id

-- test data
machine2 = factory2 [1,2,3,4]

{- tests
machine2 (\x -> x*x)
unbox2 machine2
-}

---------------------
factory2' :: [a] -> (forall b . (a -> b) -> [b])
factory2' a f = reverse $ map f a

---------------------
rotateL :: forall a . [a] -> [a]
rotateL []     = []
rotateL (x:xs) = xs ++ [x]

factory2'' :: [a] -> (forall b . (a -> b) -> [b])
factory2'' a f = rotateL $ map f a

{-
*Main> factory2' [1,2,3,4] (\x -> x*x)
[16,9,4,1]
*Main> unbox2 $ factory2' [1,2,3,4]
[4,3,2,1]
-}

{-
*Main> factory2'' [1,2,3,4] (\x -> x*x)
[4,9,16,1]
*Main> unbox2 $ factory2'' [1,2,3,4]
[2,3,4,1]
-}
