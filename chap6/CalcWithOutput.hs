module CalcWithOutput where
-- Building a Kleisli triple
-- for a program with simple output

newtype T a = T {unT :: String -> (a,String)}

-- Definition of
---- (-)^# :: Hom(A,TB) -> Hom(TA,TB)
-- plays the role of
---- return :: (Monad m) => a -> m a
---- where m = T.

eta :: a -> T a
eta x = T (\s -> (x, s))

-- Definition of
----   (-)^# :: Hom(A,TB) -> Hom(TA,TB)
-- plays the role of
---- (=<<) :: (Monad m) => (a -> m a) -> m a -> m b
---- where m = T.

sharp :: (a -> T b) -> (T a -> T b)
sharp f h = 
    T (\st -> let
                  (y,st2) = unT h st
                  (z,st3) = unT (f y) st2
              in
                  (z,st3))

-------------
-- Test data
-------------

f1 :: Integer -> T Integer
f1 x = T (\s -> (x^2, 
                 s ++ show x ++ "^2=" ++ show (x^2) ++
                 ".\n"))

f2 :: Integer -> T Float
f2 x = T (\s -> (sqrt(fromInteger x), 
                 s ++
                 "sqrt of " ++ show x ++
                 " is " ++ show (sqrt(fromInteger x)) ++
                 ".\n"))

testArgs = [(v, s) | v <- [10,100], 
                     s <- ["", "---", "hello"]]

test2f1 = [ unT (sharp eta (f1 v)) s == unT (f1 v) s |
            (v,s) <- testArgs ]

test2f2 = [ unT (sharp eta (f2 v)) s == unT (f2 v) s |
            (v,s) <- testArgs ]


-- Test1 for
---- f^# o eta_A = f
-- the theoretical goal in code
---- sharp f . eta = id
-- written extensionally
---- (sharp f . eta) a == f a
-- fully extensionally
---- unT ((sharp f . eta) (x::Int) ) (s::String) ==
---- unT (f (x::Int)) (s::String) 

test11 = unT ((sharp f1 . eta) 10) "" == unT (f1 10) ""
test11detail = unT ((sharp f1 . eta) 10)  ""

test12 = unT ((sharp f2 . eta) 10) "" == unT (f2 10) ""
test12detail = unT ((sharp f2 . eta) 10)  ""


-- Test2 for
---- eta_A^# = id_(TA)
-- the theoretical goal in code
---- sharp eta == id
-- written extensionally
---- sharp eta (ta::T a) == (ta::T a)
-- fully extensionally
---- unT (sharp eta (ta::T a)) (s::String) == 
---- unT (ta::T a) (s::String)

test21 = unT (sharp eta (f1 10)) "" ==  unT (f1 10) ""
test22 = unT (sharp eta (f2 10)) "" ==  unT (f2 10) ""


-- Test3 for
---- g^# o f^# = (g^# o f)^#
-- the theoretical goal in code
---- sharp g . sharp f == sharp (sharp g . f)
-- written extensionally
---- (sharp g . sharp f) (ta::T a) ==
---- sharp (sharp g . f) (ta::T a)
-- fully extensionally
---- unT ((sharp g . sharp f) (ta::T a)) (s::String) ==
---- unT (sharp (sharp g . f) (ta::T a)) (s::String)

test31 =  unT ((sharp f2 . sharp f1) (eta 10)) "" ==
          unT (sharp (sharp f2 . f1) (eta 10)) ""

test3 = [unT ((sharp f2 . sharp f1) (eta v)) s ==
         unT (sharp (sharp f2 . f1) (eta v)) s |
         (v,s) <- testArgs ]

-- other tests
(xTest1, stTest1) = 
    (unT $ sharp f2 $ sharp f1 $ eta 10) ""
