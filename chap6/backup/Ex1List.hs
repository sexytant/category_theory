module Ex1List where
-- The List functor
type T = ([])

--def--   eta :: A -> TA
eta :: a -> T a
eta a = [a]

--def--   (-)^# :: Hom(A,TB) -> Hom(TA,TB)
sharp :: (a -> T b) -> T a -> T b
sharp f []     = []
sharp f (x:xs) = f x ++ sharp f xs

--Q-- f^# circ eta_A = f
test2cases = [(\x->[0,x,0]),(\x -> [0,x,x,0]), (\x -> [0,x,x,x,0])]
test2 = map (\f ->(sharp f . eta) 10 == f 10) test2cases
test2detail = map (\f -> (sharp f. eta) 10) test2cases

--Q-- eta_A^# = id_(TA)
test3cases = [[],[1],[2,3],[4,5,6]]
test3 = map (\xs -> sharp eta xs == xs) test3cases
test3detail = map (\xs -> sharp eta xs) test3cases

--Q-- g^# circ f^# = (g^# circ f)^#
test4funcs = [(\x->[0,x,0]),(\x -> [0,x,x,0]), (\x -> [0,x,x,x,0])]
test4cases = [(g,f,d) | g <- test4funcs, f <- test4funcs, d <- test3cases]
test4 = map (\(g,f,d) -> (sharp g . sharp f) d == (sharp (sharp g . f)) d) test4cases
