module Continuation where
-- Building a Kleisli triple for a program of CPS
-- For simplicity, we fix the domain of the result values, namely Integer.

newtype T a = T {unT :: (a -> Int) -> Int}

unitT :: a -> T a
unitT a = T (\x -> x a)

-- f :: a -> T b
-- m :: T a
-- bDual :: b -> Int
sharpT :: (a -> T b) -> (T a -> T b)
sharpT f m = 
    T (\bDual -> unT m (\a -> unT (f a) bDual))
 
--- for tests

f :: Int -> T Int
f x = T (\c -> c (x^2))


g2 :: String -> T String
g2 x = T (\c -> c (x++x))


{-
unT (f 3) (+1) ---> 10
unT (sharpT f (unitT 3)) (+1)  ----> 10

unT (sharpT f $ sharpT f $ unitT 3) id   -----> 81

unT (g2 "hello") length -----> 5
unT (sharpT g2 (unitT "hello") ) length ----------> 10

(unT $ sharpT g2 $ sharpT g2 $ unitT "hello") length -------> 20

-}
