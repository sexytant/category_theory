module Ex2CalcWithOutput where
-- Building a Kleisli triple for a program with simple output

-- data T a = T (String -> (a,String))
newtype T a = T (String -> (a,String))

--def--  uncover the constructor
unT :: T a -> (String -> (a,String))
unT (T f) = f

--def--   eta :: A -> TA
eta :: a -> T a
eta x = T (\s -> (x, s))

--def--   (-)^# :: Hom(A,TB) -> Hom(TA,TB)
sharp :: (a -> T b) -> (T a -> T b)
sharp f h = 
    T (\st -> let
                  (y,st2) = unT h st
                  (z,st3) = unT (f y) st2
              in
                  (z,st3))

-- Test data --
f1 :: Integer -> T Integer
f1 x = T (\s -> (x^2, s ++ show x ++ "^2=" ++ show (x^2) ++ ".\n"))

f2 :: Integer -> T Integer
f2 x = T (\s -> (x+1, s ++ show x ++ "+1=" ++ show (x+1) ++ ".\n"))

g :: Integer -> T Float
g x = T (\s -> (sqrt(fromInteger x), s ++  "sqrt of " ++ show x ++ " is " ++ show (sqrt(fromInteger x)) ++ ".\n"))
--Q-- 
test1a = (unT $ eta 10) ""
(xTest1b, stTest1b) = (unT $ sharp g $ sharp f1 $ eta 10) ""

{-  You can do it in the interactive monad as follows.
let (xTest1b, stTest1b) = (unT $ sharp g $ sharp f1 $ eta 10) ""
putStr stTest1b
-}

--Q--  f^# circ eta_A = f
test2funcs  = [f1,f2]
test2ints   = [3,5,7]
test2strs   = ["bye\n","ciao\n","adios"]
test2cases  = [(f, v, s) | f <- test2funcs, v <- test2ints, s <- test2strs]
test2 = map (\(f,v,s) ->unT (sharp f $ eta v) s == unT (f v) s) test2cases

--Q-- eta_A^# = id_(TA)
test3TAs    = [T (\s -> (2, s++"hello2")), T (\s -> (3, s++"hello3"))]
test3strs   = test2strs
test3cases  = [(ta,str) | ta <- test3TAs, str <- test3strs]
test3 = map (\(ta,str) -> unT (sharp eta ta) str == unT ta str) test3cases
test3detail = map (\(ta,str) -> unT (sharp eta ta) str ) test3cases

--Q-- g^# circ f^# = (g^# circ f)^#
test4funcs = [f1,f2]
test4cases = [(g,f,d) | g <- test4funcs, f <- test4funcs, d <- test3TAs]
test4 = map (\(g,f,d) -> unT ((sharp g . sharp f) d) "" == unT (sharp (sharp g . f) d) "") test4cases


