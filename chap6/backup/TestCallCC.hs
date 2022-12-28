module TestCallCC where
import Control.Monad.Cont

{- definitions in the library
cont :: ((a -> r) -> r) -> Cont r a
runCont :: Cont r a -> (a -> r) -> r

instance Monad (Cont r) where
  return x = cont ($ x)
  -- return x = cont (\k -> k x)
  s >>= f = cont $ \c -> runCont s $ \x -> runCont (f x) c
-}

foo :: Int -> Cont r String
foo x = callCC $ \k -> do
  let y = x^2+3
  when (y>20) $ k "over twenty"
  return (show $ y-4)

foo2 :: String -> Cont r String
foo2 st = cont (\k -> k (st++st))

foo12 x =
  do st <- foo x
     y <- foo2 st
     return y

{- 
*TestCallCC> runCont (foo 100) length
11
*TestCallCC> runCont (foo 100) id
"over twenty"
*TestCallCC> runCont (foo 0) id
"-1"

*TestCallCC> runCont (foo12 10) id
"over twentyover twenty"
*TestCallCC> runCont (foo12 0) id
"-1-1"

-}

bar :: Char -> String -> Cont r Int
bar c s = do
   msg <- callCC $ \k -> do
      let s0 = c : s
      when (s0 == "hello") $ k "They say hello."
      let s1 = show s0
      return ("They appear to be saying " ++ s1)
   return (length msg)

{-
*TestCallCC> (runCont $ bar 'h' "ello") show
"15"
*TestCallCC> (runCont $ bar 'h' "ell") show
"31"
-}

bar2 :: Char -> String -> Cont r String
bar2 c s = do
   msg <- callCC $ \k -> do
      let s0 = c : s
      when (s0 == "hello") $ k "They say hello."
      let s1 = show s0
      return ("They appear to be saying " ++ s1)
   return msg

{-
*TestCallCC> (runCont $ bar2 'h' "ello") id
"They say hello."
*TestCallCC> (runCont $ bar2 'h' "ell") id
"They appear to be saying \"hell\""
-}