module Ex4Maybe where
-- Building a Kleisli triple for the Maybe functor

type T = Maybe
-- As you know, the Maybe functor has two data constructors.
-- So, the presentation is a little bit different from Ex1 to Ex3.

--def--
eta :: a -> T a
eta = Just

--def--
-- f :: a -> T b
sharp :: (a -> T b) -> (T a -> T b)
sharp f Nothing  = Nothing
sharp f (Just x) = f x
    
--- for tests
f1 :: Float -> T Float
f1 0 = Nothing
f1 x = Just (1.0/x)

f2 :: Float -> T Float
f2 x = Just (x+1)

--Q--  f^# circ eta_A = f
test2funcs  = [f1,f2]
test2floats   = [10.0,100.0,0.1] :: [Float]
test2cases  = [(f, v) | f <- test2funcs, v <- test2floats]
test2 = map (\(f,v) -> (sharp f $ eta v) ==  f v) test2cases

--Q-- eta_A^# = id_(TA)
test3TAs    = [Nothing, Just 0.0, Just 1.0]
test3 = map (\ta -> sharp eta ta == ta) test3TAs

--Q-- g^# circ f^# = (g^# circ f)^#
test4funcs = [f1,f2]
test4TAs   = test3TAs
test4cases = [(g,f,ta) | 
              g <- test4funcs, f <- test4funcs, ta <- test4TAs]
test4 = map (\(g,f,ta) -> 
              (sharp g . sharp f) ta == sharp (sharp g . f) ta) 
            test4cases
test4detail1 = 
        map (\(g,f,ta) -> 
              (sharp g . sharp f) ta)
              test4cases
test4detail2 =
        map (\(g,f,ta) -> 
              sharp (sharp g . f) ta)
            test4cases

