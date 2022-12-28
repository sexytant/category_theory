module Continuation2 where
-- Building a Kleisli triple for a program of CPS
-- In this version, T takes two arguments.

newtype T v a = T {runT :: (a -> v) -> v}


unitT :: a -> T v a
unitT a = T (\x -> x a)

-- f :: a -> T v b
-- m :: T v a
-- b' :: b -> v
extT :: (a -> T v b) -> (T v a -> T v b)
extT f m = 
    T (\b' -> runT m (\a -> runT (f a) b'))
 
--- for tests

f :: Int -> T Int Int
f x = T (\c -> c (x^2))


g2 :: String -> T Int String
g2 x = T (\c -> c (x++x))


{-
runT (f 3) (+1) ---> 10
runT (extT f (unitT 3)) (+1)  ----> 10

runT (extT f $ extT f $ unitT 3) id   -----> 81

runT (g2 "hello") length -----> 5
runT (extT g2 (unitT "hello") ) length ----------> 10

(runT $ extT g2 $ extT g2 $ unitT "hello") length -------> 20

-}
