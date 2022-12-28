module Maybe where
-- Building a Kleisli triple for the Maybe functor

type T = Maybe

-- Definition of
---- eta :: A -> TA
-- plays the role of
---- return :: (Monad m) => a -> m a
---- where m = T.

eta :: a -> T a
eta = Just

-- Definition of
---- (-)^# :: Hom(A,TB) -> Hom(TA,TB)
-- plays the role of
---- (=<<) :: (Monad m) => (a -> m a) -> m a -> m b
---- where m = T.
-- Types of arguments in the definition below
---- f :: a -> T b

sharp :: (a -> T b) -> (T a -> T b)
sharp f Nothing  = Nothing
sharp f (Just x) = f x


-------------
-- Test data
-------------

f1 :: Float -> T Float
f1 0 = Nothing
f1 x = Just (1.0/x)

f2 :: Float -> T Float
f2 x = Just (x+1)

testFuncs  = [f1,f2]
testFloats   = [10.0,100.0,0.1] :: [Float]
testTAs    = [Nothing, Just 0.0, Just 1.0]


-- Test1 for
---- f^# o eta_A = f
-- the theoretical goal in code
---- sharp f . eta == f
-- written extensionally
---- (sharp f . eta) a == f a

test1cases  = [(f, v) | f <- testFuncs, v <- testFloats]
test1 =
    map (\(f,v) -> (sharp f . eta) v ==  f v) test1cases

-- Test2 for
---- eta_A^# = id_(TA)
-- the theoretical goal in code
---- sharp eta == id
-- written extensionally
---- sharp eta (ta::T a) == (ta::T a)

test2 = map (\ta -> sharp eta ta == ta) testTAs

-- Test3 for
---- g^# o f^# = (g^# o f)^#
-- the theoretical goal in code
---- sharp g . sharp f == sharp (sharp g . f)
-- written extensionally
---- (sharp g . sharp f) (ta::T a) ==
---- sharp (sharp g . f) (ta::T a)

test3cases = [(g,f,ta) | 
              g <- testFuncs, f <- testFuncs,
              ta <- testTAs]

test3 = map (\(g,f,ta) -> 
              (sharp g . sharp f) ta ==
              sharp (sharp g . f) ta) 
            test3cases

test3detail1 = 
        map (\(g,f,ta) -> 
              (sharp g . sharp f) ta)
              test3cases

test3detail2 =
        map (\(g,f,ta) -> 
              sharp (sharp g . f) ta)
            test3cases
