module Continuation where
-- Building a Kleisli triple for a program of CPS
-- In this version, T takes two arguments.

newtype T v a = T {unT :: (a -> v) -> v}

-- Definition of eta
---- eta :: A -> TA
-- plays the role of
---- return :: (Monad m) => a -> m a
---- where m = T v.

eta :: a -> T v a
eta a = T (\x -> x a)

-- Definition of sharp
---- (-)^# :: Hom(A,TB) -> Hom(TA,TB)
-- plays the role of
---- (=<<) :: (Monad m) => (a -> m a) -> m a -> m b
---- where m = T v.
-- Types of arguments in the definition below
---- f :: a -> T v b
---- m :: T v a
---- b' :: b -> v

sharp :: (a -> T v b) -> (T v a -> T v b)
sharp f m = 
    T (\b' -> unT m (\a -> unT (f a) b'))

-------------
-- Test data
-------------

f1 :: Int -> T Int Int
f1 x = T (\c -> c (x+1))

f2 :: Int -> T Int Int
f2 x = T (\c -> c (x^2))

g1 :: String -> T Int String
g1 x = T (\c -> c (x++"!"))

g2 :: String -> T Int String
g2 x = T (\c -> c (x++x))

-- Test1 for
---- f^# o eta_A = f
-- the theoretical goal in code
---- sharp f . eta == f
-- written extensionally
---- (sharp f . eta) a == f a
-- fully extensionally
---- unT ((sharp f . eta) a) (h::a->v) ==
---- unT (f a) (h::a->v)

test111 = unT ((sharp f1 . eta) 3) (^2) == unT (f1 3) (^2)
test112 = unT ((sharp f2 . eta) 3) (+1) == unT (f2 3) (+1)
test121 = unT ((sharp g1 . eta) "hello") length ==
          unT (g1 "hello") length
test122 = unT ((sharp g2 . eta) "hello") length ==
          unT (g2 "hello") length
test129 = unT ((sharp g2 . eta) "hello") ((^2) . length) ==
          unT (g2 "hello") ((^2) . length)


-- Test2 for
---- eta_A^# = id_(TA)
-- the theoretical goal in code
---- sharp eta == id
-- written extensionally
---- sharp eta (ta::T a) == (ta::T a)
-- fully extensionally
---- unT (sharp eta (ta::T a)) (f::a->v) == 
---- unT (ta::T a) (f::a->v)

test211 = unT (sharp eta (eta 3)) (+1) == unT (eta 3) (+1)
test212 = unT (sharp eta (eta 3)) (^2) == unT (eta 3) (^2)
test221 = unT (sharp eta (f1 3)) (+1) == unT (f1 3) (+1)
test222 = unT (sharp eta (f2 3)) (+1) == unT (f2 3) (+1)
test231 = unT (sharp eta (g1 "hello")) length ==
          unT (g1 "hello") length
test232 = unT (sharp eta (g2 "hello")) length ==
          unT (g2 "hello") length


-- Test3 for
---- g^# o f^# = (g^# o f)^#
-- the theoretical goal in code
---- sharp g . sharp f == sharp (sharp g . f)
-- written extensionally
---- (sharp g . sharp f) (ta::T a) ==
---- sharp (sharp g . f) (ta::T a)
-- fully extensionally
---- unT ((sharp g . sharp f) (ta::T a)) (h::a->v) ==
---- unT (sharp (sharp g . f) (ta::T a)) (h::a->v)

test311 = 
    unT ((sharp f2 . sharp f1) (eta 3)) (+1) ==
    unT ((sharp (sharp f2 . f1)) (eta 3)) (+1)
test312 =
    unT ((sharp f2 . sharp f1) (eta 3)) (^2) ==
    unT ((sharp (sharp f2 . f1)) (eta 3)) (^2)
test321 =
    unT ((sharp g2 . sharp g1) (eta "hello")) length ==
    unT ((sharp (sharp g2 . g1)) (eta "hello")) length
test322 =
    unT ((sharp g2 . sharp g1) (g1 "hello")) length ==
    unT ((sharp (sharp g2 . g1)) (g1 "hello")) length
