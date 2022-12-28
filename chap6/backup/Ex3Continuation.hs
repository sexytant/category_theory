module Ex3Continuation where
-- Building a Kleisli triple for a program of CPS
-- For simplicity, we fix the domain of the result values, namely Integer.
newtype T a = T {unT :: (a -> Int) -> Int}

--def--
eta :: a -> T a
eta a = T (\x -> x a)

--def--
-- f :: a -> T b
-- m :: T a
-- bDual :: b -> Int
sharp :: (a -> T b) -> (T a -> T b)
sharp f m = 
    T (\bDual -> unT m (\a -> unT (f a) bDual))
 
--- for tests
f1 :: Int -> T Int
f1 x = T (\c -> c (x^2))

f2 :: Int -> T Int
f2 x = T (\c -> c (x+1))

g2 :: String -> T String
g2 x = T (\c -> c (x++x))

{- Tests
unT (f1 3) (+1) ---> 10
unT (sharp f1 (eta 3)) (+1)  ----> 10

unT (sharp f1 $ sharp f1 $ eta 3) id   -----> 81

unT (g2 "hello") length -----> 5
unT (sharp g2 (eta "hello") ) length ----------> 10

(unT $ sharp g2 $ sharp g2 $ eta "hello") length -------> 20

-}

--Q--  f^# circ eta_A = f
test2funcs  = [f1,f2]
test2ints   = [10,100,1000] :: [Int]
test2fInits   = [id, (\x -> x-1)]
test2cases  = [(f, v, fi) | f <- test2funcs, v <- test2ints, fi <- test2fInits]
test2 = map (\(f,v,fi) ->unT (sharp f $ eta v) fi == unT (f v) fi) test2cases

--Q-- eta_A^# = id_(TA)
test3TAs    = [T (\c -> c 10), T (\c -> c 100), T (\c -> c 1000)]
test3fInits   = test2fInits
test3cases  = [(ta,fi) | ta <- test3TAs, fi <- test3fInits]
test3 = map (\(ta,fi) -> unT (sharp eta ta) fi == unT ta fi) test3cases
test3detail = map (\(ta,fi) -> unT (sharp eta ta) fi ) test3cases

--Q-- g^# circ f^# = (g^# circ f)^#
test4funcs = [f1,f2]
test4TAs   = test3TAs
test4fInits = test2fInits
test4cases = [(g,f,ta,fi) | 
              g <- test4funcs, f <- test4funcs, ta <- test4TAs, fi <- test4fInits]
test4 = map (\(g,f,ta,fi) -> 
              unT ((sharp g . sharp f) ta) fi == unT (sharp (sharp g . f) ta) fi) 
            test4cases
