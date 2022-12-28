module ListMonadTest where

test1 = [(x,y) | x <- [1,2,3,4], y <- "abc"]

test2 = do
  x <- [1,2,3,4]
  y <- "abc"
  return (x,y)

test3 = [1,2,3,4] >>= \x -> "abc" >>= \y -> return (x,y)

test4 = zip [1..] "abc"

test5 = take 5 $ zip [0..] [10..]

test6 = take 5 $ [(x,y) | x <- [0..], y <- [10..] ]
-- compare the results of test5 and test6
