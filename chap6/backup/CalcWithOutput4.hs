module CalcWithOutput4 where
-- Building a Kleisli triple for a program with simple output
-- Type constructor T takes two arguments.

newtype T st a = T {unT :: st -> (a,st)}


unitT :: a -> T st a
unitT x = T (\s -> (x, s))

-- (=<<) :: (Monad m) => (a -> m a) -> m a -> m b
-- We take m = T st.
extT :: (a -> T st b) -> (T st a -> T st b)
extT f h = 
    T (\st1 -> let
                   (y,st2) = unT h st1
                   (z,st3) = unT (f y) st2
               in
                   (z,st3))

----- test data
f :: Integer -> T String Integer
f x = T (\s -> (x^2, s ++ show x ++ "^2=" ++ show (x^2) ++ ".\n"))

g :: Integer -> T String Float
g x = T (\s -> (sqrt(fromInteger x), s ++  "sqrt of " ++ show x ++ " is " ++ show (sqrt(fromInteger x)) ++ ".\n"))


{-
:kind T String Int
-----> T String Int :: *
:kind T String
-----> T String :: * -> *




(unT $ unitT 10) ""

let (xTest1, stTest1) = (unT $ extT g $ extT f $ unitT 10) ""
putStr stTest1
-}
