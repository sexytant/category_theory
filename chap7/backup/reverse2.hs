{-# LANGUAGE RankNTypes #-}

check2 :: [a] -> (forall b . (a -> b) -> [b])
check2 a f = map f a

uncheck2 :: (forall b . (a -> b) -> [b]) -> [a]
uncheck2 t = t id

-- test data
machine2 = check2 [1,2,3,4]

{- tests
machine2 (\x -> x*x)
uncheck2 machine2
-}

---------------------
check2' :: [a] -> (forall b . (a -> b) -> [b])
check2' a f = reverse $ map f a

---------------------
rotateL :: forall a . [a] -> [a]
rotateL []     = []
rotateL (x:xs) = xs ++ [x]

check2'' :: [a] -> (forall b . (a -> b) -> [b])
check2'' a f = rotateL $ map f a

{-
*Main> check2' [1,2,3,4] (\x -> x*x)
[16,9,4,1]
*Main> uncheck2 $ check2' [1,2,3,4]
[4,3,2,1]
-}

{-
*Main> check2'' [1,2,3,4] (\x -> x*x)
[4,9,16,1]
*Main> uncheck2 $ check2'' [1,2,3,4]
[2,3,4,1]
-}
