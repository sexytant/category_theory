module MonadTree where
import Control.Monad
import Data.List.Split

data T a = N a | T (T a) (T a) deriving Show


instance Functor T where
  fmap f (N a)     = N (f a)
  fmap f (T t1 t2) = T (fmap f t1) (fmap f t2)

  -- fmap :: (a -> b) -> T a -> T b


instance Applicative T where
  pure x = N x
  tf <*> ta =
    case tf of
      N f     -> fmap f ta
      T f1 f2 -> T (f1 <*> ta) (f2 <*> ta)

  -- pure :: a -> T a
  -- (<*>) :: T (a->b) -> T a -> T b


instance Monad T where
  return = pure
  h >>= f = sharpT f h

  -- return :: a -> T a
  -- (>>=) :: T a -> (a -> T b) -> T b


sharpT :: (a -> T b) -> (T a -> T b)
sharpT f h =
  case h of
    N a     -> f a
    T t1 t2 -> T (sharpT f t1) (sharpT f t2)

---------------
--- tests
---------------

test1 :: T String
test1 = T (T (N "Hello") (N "World")) 
          (T (N "Black") (T (N "Jack") (N "ThisIsALongWord")))

-- fmap length test1
-- length <$> test1

-- pure 1 :: T Int
-- pure 1
-- These two give different results.
-- We cannot predict the context which the intereter assumes.

test2 :: T (String -> Int)
test2 = T (N length) (N (\_ -> 100))

-- 
-- test2 <*> test1


-- Left associative tree generation

f3 :: String -> T String
f3 s = f3a $ splitOn " " s

f3a :: [String] -> T String
f3a ss = f3aa $ reverse ss

f3aa [s]    = N s
f3aa (s:ss) = T (f3aa ss) (N s)

-- f3 "hello world I am happy"

-- 
test3h = T (N "x y z") (N "a b c")

-- test3h >>= f3

