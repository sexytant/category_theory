module CalcWithOutput4StateMonad where
import Control.Monad.Trans.State


----- test data
f :: Integer -> State String Integer
f x = state (\s -> (x^2, s ++ show x ++ "^2=" ++ show (x^2) ++ ".\n"))

g :: Integer -> State String Float
g x = state (\s -> (sqrt(fromInteger x), s ++  "sqrt of " ++ show x ++ " is " ++ show (sqrt(fromInteger x)) ++ ".\n"))


{-
:kind State String Int
-----> State String Int :: *
:kind State String
-----> State String :: * -> *

runState (return 10) ""

let (xTest1, stTest1) = runState ( return 10 >>= f >>= g ) ""
putStr stTest1

-}

-- test on do-notation
test4 =
  do
    x <- return 10
    y <- f x
    g y

-- runState test4 ""