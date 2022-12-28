module Continuation2myMonad where
import Control.Monad
-- In this version, T takes two arguments.

newtype T v a = T {unT :: (a -> v) -> v}

-- instance declarations

instance Functor (T v) where
  fmap f h =
    T (\bDual -> unT h (bDual.f))
  -- fmap  :: (a -> b) -> (T v a -> T v b)
  -- f     ::  a -> b
  -- unT h :: (a -> v) -> v
  -- bDual ::  b -> v
  -- result::  v


instance (Applicative (T v)) where
  pure x = T (\aDual -> aDual x)
  -- pure :: a -> T v a
  tf <*> ta = 
     T (\bDual -> unT tf $ \f -> unT ta (bDual.f))
  -- (<*>) :: (T v (a -> b)) -> T v a -> T v b
  -- unT tf :: ((a->b)->v) -> v
  -- unT ta :: (  a   ->v) -> v
  -- bDual  :: (  b   ->v)


instance (Monad (T v)) where
  return = pure
  h >>= f = sharpT f h
  --  return :: a -> T v a
  --  (>>=) :: T v a -> (a -> T v b) -> (T v b)


-- f :: a -> T v b
-- m :: T v a
-- bDual :: b -> v
sharpT :: (a -> T v b) -> (T v a -> T v b)
sharpT f m = 
    T (\bDual -> unT m (\a -> unT (f a) bDual))


--- for tests

f :: Int -> T Int Int
f x = T (\c -> c (x^2))


g2 :: String -> T Int String
g2 x = T (\c -> c (x++x))

test9 =
  do x <- return "hello"
     y <- g2 x
     g2 y

-- unT test9 length

{-
unT (f 3) (+1) ---> 10
unT (sharpT f (unitT 3)) (+1)  ----> 10

unT (sharpT f $ sharpT f $ unitT 3) id   -----> 81

unT (g2 "hello") length -----> 5
unT (sharpT g2 (unitT "hello") ) length ----------> 10

(unT $ sharpT g2 $ sharpT g2 $ unitT "hello") length -------> 20

unT (return "hello" >>= g2 >>= g2) length

-}
