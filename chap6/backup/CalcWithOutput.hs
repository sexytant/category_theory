module CalcWithOutput where
-- Building a Kleisli triple for a program with simple output

-- data T a = T (String -> (a,String))
newtype T a = T (String -> (a,String))

-- uncover the constructor
unT :: T a -> (String -> (a,String))
unT (T f) = f

f :: Integer -> T Integer
f x = T (\s -> (x^2, s ++ show x ++ "^2=" ++ show (x^2) ++ ".\n"))

g :: Integer -> T Float
g x = T (\s -> (sqrt(fromInteger x), s ++  "sqrt of " ++ show x ++ " is " ++ show (sqrt(fromInteger x)) ++ ".\n"))

unitT :: a -> T a
unitT x = T (\s -> (x, s))

-- (=<<) :: (Monad m) => (a -> m a) -> m a -> m b
-- We take m = T.
extT :: (a -> T b) -> (T a -> T b)
extT f h = 
    T (\st -> let
                  (y,st2) = unT h st
                  (z,st3) = unT (f y) st2
              in
                  (z,st3))
 

{-
(unT $ unitT 10) ""

let (xTest1, stTest1) = (unT $ extT g $ extT f $ unitT 10) ""
putStr stTest1
-}
