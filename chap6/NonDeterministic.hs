module NonDeterministic where
-- The List functor

type T = ([])

-- Definition of
---- eta :: A -> TA
-- plays the role of
---- return :: (Monad m) => a -> m a
---- where m = T.

eta :: a -> T a
eta a = [a]

-- Definition of
---- (-)^# :: Hom(A,TB) -> Hom(TA,TB)
-- plays the role of
---- (=<<) :: (Monad m) => (a -> m a) -> m a -> m b
---- where m = T.

sharp :: (a -> T b) -> T a -> T b
sharp f []     = []
sharp f (x:xs) = f x ++ sharp f xs

-------------
-- Test data
-------------

testFuncs = [(\x -> [0,x,0]),
             (\x -> [0,x,x,0]), 
             (\x -> [0,x,x,x,0])]

-- Test1 for
---- f^# o eta_A = f
-- the theoretical goal in code
---- sharp f . eta == f
-- written extensionally
---- (sharp f . eta) a == f a

test1 = map (\f ->(sharp f . eta) 10 == f 10) testFuncs
test1detail = map (\f -> (sharp f. eta) 10) testFuncs


-- Test2 for
---- eta_A^# = id_(TA)
-- the theoretical goal in code
---- sharp eta == id
-- written extensionally
---- sharp eta (ta::T a) == (ta::T a)

testLists = [[],[1],[2,3],[4,5,6]]

test2 = map (\xs -> sharp eta xs == xs) testLists
test2detail = map (\xs -> sharp eta xs) testLists


-- Test3 for
---- g^# o f^# = (g^# o f)^#
-- the theoretical goal in code
---- sharp g . sharp f == sharp (sharp g . f)
-- written extensionally
---- (sharp g . sharp f) (ta::T a) ==
---- sharp (sharp g . f) (ta::T a)

test3cases = [(g,f,d) | g <- testFuncs, 
                        f <- testFuncs, 
                        d <- testLists]

test3 = map (\(g,f,d) -> 
                (sharp g . sharp f) d ==
                (sharp (sharp g . f)) d) 
            test3cases
