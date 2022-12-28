module Continuation2 where
import Control.Monad
import Continuation

-- After building a Kleisli triple
-- for a continuation,
-- we declare instances of
-- Functor, Applicative, and Monad

-----------
-- Functor
-----------

---- instance declarations

-- T v a is a bifunctor.
-- However, we declare a Fuctor instance only for (T v).

instance Functor (T v) where
  fmap f h =
    T (\b' -> unT h (b'.f))

  -- where
  -- fmap  :: (a -> b) -> (T v a -> T v b)
  -- f     ::  a -> b
  -- unT h :: (a -> v) -> v
  -- b' ::  b -> v
  -- result::  v

{- We have to prove the following functor rules.
Prop.
  fmap id = id, or in pointful style, 
  fmap id h = h.
Proof.
  T (\b' -> unT h (b'.id)) 
    = T (\b' -> unT h b') 
    -- eta conversion
    = T $ unT h 
    -- cancel T/unT pair
    = h
  QED.

Prop.
  fmap (g.f) == fmap g . fmap f, or in pointful style
  fmap (g.f) h == fmap g (fmap f h).
Proof.
  Let
    f :: a -> b
    g :: b -> c
    unT h :: (a -> v) -> v
    c' :: (c -> v)
    b' :: (b -> v)

  -- LHS
  fmap (g.f) h = T (\c' -> unT h (c'.g.f))

  -- RHS
  fmap g (fmap f h) 
    = fmap g (T (\b' -> unT h (b'.f))
    = T (\c' -> unT (T (\b' -> unT h (b'.f))) (c'.g))
    -- cancel unT/T pair
    = T (\c' -> (\b' -> unT h (b'.f)) (c'.g))
    -- beta reduction
    = T (\c' -> unT h (c'.g.f))
  QED.
-}

-- Note that T itself is not an Applicative, but (T v) is.

instance (Applicative (T v)) where
  pure = eta
  -- pure :: a -> T v a
  tf <*> ta = 
     T (\b' -> unT tf $ \f -> unT ta (b'. f))
  -- where
  -- (<*>)   :: (T v (a -> b)) -> T v a -> T v b
  -- unT tf :: ((a->b)->v) -> v
  -- unT ta :: (  a   ->v) -> v
  -- b'      ::    b   ->v
  -- f       ::    a   ->b
  -- b'.f    ::    a   ->v

{- We have to prove the following Applicative rules.
Prop.[identity]
  pure id <*> ta = ta
Proof.
  pure id <*> ta
    = eta id <*> ta
    = T (\aa' -> aa' id) <*> ta
    = T (\b' -> unT (T (\aa' -> aa' id)) $ \f -> unT ta (b'.f))
    -- cancel unT/T pair
    = T (\b' -> (\aa' -> aa' id) $ \f -> unT ta (b'.f))
    -- beta reduction
    = T (\b' -> (\f -> unT ta (b'.f)) id)
    -- beta reduction
    = T (\b' -> unT ta (b'.id))
    = T (\b' -> unT ta b')
    -- eta conversion
    = T (unT ta)
    -- cancel T/unT pair
    = ta

  -- where
  -- unT ta :: ((a->b)->v) -> v
  -- aa'     ::  (a->a)->v
  QED.

Prop.[composition]
  pure (.) <*> tu <*> tv <*> tw = tu <*> (tv <*> tw)
  --note--  (.) u v w = (u.v) w = u (v w)
Proof.
---------
-- LHS
---------

  -- (.)     :: (b->c) -> (a->b) -> (a->c)
  (.) = \g -> \f -> \x -> g (f x)

  -- bcabac'  :: ((b->c) -> (a->b) -> (a->c)) -> v
  -- pure (.) :: T v ((b->c) -> (a->b) -> (a->c)) 
  pure (.) = T (\bcabac' -> bcabac' (.))

  -- tu              :: T v (b->c)
  -- unT tu          :: ((b->c)->v) -> v
  -- pure (.) <*> tu :: T v ((a->b) -> (a->c))
  -- abac'           :: ((a->b) -> (a->c)) -> v
  -- abac'.z         :: (b->c)->v
  -- z               :: (b->c) -> ((a->b) -> (a->c))
  pure (.) <*> tu
    = eta (.) <*> tu
    = T ( \bcabac' -> bcabac' (.) ) <*> tu
    = T ( \abac' -> unT (T ( \bcabac' -> bcabac' (.) )) $ \z -> unT tu $ abac'.z )
    = T ( \abac' -> (\bcabac' -> bcabac' (.)) $ \z -> unT tu $ abac'.z )
    = T ( \abac' -> (\z -> unT tu $ abac'.z) (.) )
    = T ( \abac' -> unT tu $ abac'.(.) )
    = T ( \abac' -> unT tu $ abac'.(\u -> \v -> \x -> u (v x)) )
    = T ( \abac' -> unT tu $ \u -> abac' $ \v -> \x -> u (v x) )

  -- tv              :: T v (a->b)
  -- unT tv          :: ((a->b)->v) -> v
  -- pure (.) <*> tu <*> tv :: T v (a->c)
  -- g               :: b->c

  pure (.) <*> tu <*> tv
    = T ( \abac' -> unT tu $ \u -> abac' $ \v -> \x -> u (v x) ) <*> tv
    = T ( \ac' -> \abac' -> unT tu $ \u -> abac' $ \v -> \x -> u (v x)
                      $ \g -> unT tv (ac'.g)  )
    = T ( \ac' -> unT tu $ \u -> (\g -> unT tv (ac'.g)) $ \v -> \x -> u (v x) )
    = T ( \ac' -> unT tu $ \u -> unT tv $ ac'.(\v -> \x -> u (v x)) )

  -- tw                            :: T v a
  -- pure (.) <*> tu <*> tv <*> tw :: T v c
  --
  pure (.) <*> tu <*> tv <*> tw
    = T (c' -> \ac' -> unT tu $ \u -> unT tv $ ac'.(\v -> \x -> u (v x)) )
                    $ f -> unT tw (c'.f)
    = T (c' -> unT tu $ \u -> unT tv $ (f -> unT tw (c'.f)).(\v -> \x -> u (v x)) )
    = T (c' -> unT tu $ \u -> unT tv $ \v -> unT tw $ c'.(\x -> u (v x))  )
    = T (c' -> unT tu $ \u -> unT tv $ \v -> unT tw $ \x -> c' $ u (v x)  )
    = T (c' -> unT tu $ \u -> unT tv $ \v -> unT tw $ c'.u.v)

---------
-- RHS
--------
  -- tv        :: T v (a->b)
  -- unT tv    :: ((a->b)->v) -> v
  -- tw        :: T v a
  -- unT tw    :: (a->v) -> v
  -- tv <*> tw :: T v b
  -- b'        :: b -> v
  -- v         :: a -> b
  tv <*> tw
    = T (\b' -> unT tv $ \v -> unT tw (b'.v))

  -- tu <*> (tv <*> tw) :: T v c
  -- c'                 :: c -> v
  -- b'                 :: b -> v
  -- u                  :: b -> c
  tu <*> (tv <*> tw)
    = T (\c' -> unT tu $ \u -> unT (T (\b' -> unT tv 
                                 $ \v -> unT tw (b'.v))) (c'.u) )
    = T (\c' -> unT tu $ \u -> (\b' -> unT tv $ \v -> unT tw (b'.v)) (c'.u) ) )
    = T (\c' -> unT tu $ \u -> unT tv $ \v -> unT tw $ c'.u.v )
  QED.

Prop.[homomorphism]
  pure f <*> pure x == pure (f x)
Proof.
  -- f          :: a -> b
  -- x          :: a
  -- pure f     :: T v (a->b)
  -- pure x     :: T v a
  -- pure (f x) :: T v b
  -- ab'        :: (a->b) -> v
  -- b'         :: b -> v
  -- a'         :: a -> v
  -- g          :: a -> b
  pure f <*> pure x
    = T (\ab'->ab' f) <*> T (\a'->a' x)
    = T (b' -> unT (T (\ab'->ab' f)) $ \g -> unT (T (\a'->a' x)) (b'.g) )
    = T (b' -> (\ab'->ab' f) $ \g -> (\a'->a' x) (b'.g) )
    = T (b' -> (\ab'->ab' f) $ \g -> (b'.g) x )
    = T (b' -> (\g -> (b'.g) x ) f )
    = T (b' -> (b'.f) x )
    = T (b' -> b' (f x) )
    = pure (f x)
  QED.

Prop.[iterchange]
  u <*> pure y == pure ($ y) <*> u
Poof.
  -- u            :: T v (a->b)
  -- y            :: a
  -- pure y       :: T v a
  -- u <*> pure y :: T v b
  -- a'           :: a -> v
  -- b'           :: b -> v
  -- g            :: a -> b
  u <*> pure y
    = u <*> T (\a'->a' y)
    = T (\b' -> unT u $ \g -> unT (T (\a'->a' y)) (b'.g) )
    = T (\b' -> unT u $ \g -> (\a'->a' y) (b'.g) )
    = T (\b' -> unT u $ \g -> (b'.g) y )
    = T (\b' -> unT u $ \g -> b' (g y) )

  -- $ y          :: (a->v) -> v
  pure ($ y) <*> u
    = pure (\f -> f y) <*> u
    = T (\g -> g (\f -> f y)) <*> u
    = T (\b' -> unT (T (\g -> g (\f -> f y))) $ \h -> unT u (b'.h) )
    = T (\b' -> (\g -> g (\f -> f y)) $ \h -> unT u (b'.h) )
    = T (\b' -> (\h -> unT u (b'.h)) (\f -> f y) )
    = T (\b' -> unT u $ b'.(\f -> f y) )
    = T (\b' -> unT u $ \f -> b' (f y) )
  QED.
-}


-- Note that T itself is not a Monad, but (T v) is.
instance (Monad (T v)) where
  return = pure
  h >>= f = sharp f h
  --  return :: a -> T v a
  --  (>>=) :: T v a -> (a -> T v b) -> (T v b)

{-
Prop. [left identity]
  return x >>= f  = f x
Proof.
  f  :: a -> T v b
  b' :: b -> v
  return x >>= f
    = T (\a' -> a' x) >>= f
    = sharp f $ T (\a' -> a' x)
    = T (\b' -> unT (T (\a' -> a' x)) $ \a -> unT (f a) b' )
    = T (\b' -> (\a' -> a' x) $ \a -> unT (f a) b' )
    = T (\b' -> unT (f x) b')
    = T $ unT (f x)
    = f x
  QED.

Prop. [right identity]
  m >>= return = m
Proof.
  m      :: T v a
  return :: a -> T v a
  m >>= return
    = sharp return m
    = T (\a' -> unT m $ \a -> unT (return a) a' )
    = T (\a' -> unT m $ \a -> unT (T (\f -> f a)) a' )
    = T (\a' -> unT m $ \a -> (\f -> f a) a' )
    = T (\a' -> unT m $ \a -> a' a )
    = T (\a' -> unT m a')
    = T (unT m)
    = m
  QED.

Prop. [associativity]
  (m >>= f) >>= g  =  m >>= (\x -> f x >>= g)
Proof.
  m   :: T v a
  f   :: a -> T v b
  g   :: b -> T v c
  (m >>= f) >>= g
    = (sharp f m) >>= g
    = sharp g (sharp f m)
    = sharp g $ T (\b' -> unT m $ \a -> unT (f a) b')
    = T (\c' -> (unT (T (\b' -> unT m $ \a -> unT (f a) b')))
                  \b -> unT (g b) c' )
    = T (\c' -> (\b' -> unT m $ \a -> unT (f a) b')
                  \b -> unT (g b) c' )
    = T (\c' -> unT m $ \a -> unT (f a) $ \b -> unT (g b) c' )

  -- x   :: a
  -- f x :: T v b
  -- g   :: b -> T v c
  f x >>= g
    = sharp g (f x)
    = T (\c' -> unT (f x) (\b -> unT (g b) c') )

  -- (\x -> f x >>= g) :: a -> T v c
  -- m                 :: T v a
  -- c', d'            :: c -> v
  m >>= (\x -> f x >>= g)
    = sharp (\x -> T (\c' -> unT (f x) (\b -> unT (g b) c') )) m
    = T (\d' -> unT m $ 
                 \a -> unT ( T (\c' -> unT (f a) (\b -> unT (g b) c')))) d')
    = T (\d' -> unT m $ 
                 \a -> (\c' -> unT (f a) (\b -> unT (g b) c')) d')
                       -----------------------------------------
    = T (\d' -> unT m $
                 \a -> (unT (f a) (\b -> unT (g b) d')) )
    = T (\d' -> unT m $ \a -> unT (f a) $ \b -> unT (g b) d' )
  QED.

-}


--- for tests

{- defined in Continuation.hs
f2 :: Int -> T Int Int
f2 x = T (\c -> c (x^2))

g2 :: String -> T Int String
g2 x = T (\c -> c (x++x))
-}

test9 =
  do x <- return "hello"
     y <- g2 x
     g2 y

-- unT test9 length

{-
unT (f2 3) (+1) ---> 10
unT (return 3 >>= f2) (+1)
unT (sharp f2 (eta 3)) (+1)

unT (sharp f2 $ sharp f2 $ eta 3) id   -----> 81
unT (return 3 >>= f2 >>= f2) id

unT (g2 "hello") length -----> 10
unT (sharp g2 (eta "hello") ) length
unT (return "hello" >>= g2) length

(unT $ sharp g2 $ sharp g2 $ eta "hello") length -------> 20
unT (return "hello" >>= g2 >>= g2) length
-}


-------------
-- Pythagoras
-------------

sq' :: (Num a) => a -> T v a
sq' x = return (x*x)
-- sq' x = T (\f -> f (x*x))
-- unT (sq' 3) id ----------> 9

add' :: (Num a) => a -> a -> T v a
add' x y = return (x+y)
-- add' x y = T (\f -> f (x+y))
-- unT (add' 11 19) id ----------> 30

sqrt' :: (Floating a) => a -> T v a
sqrt' x = return (sqrt x)
-- sqrt' x = T (\f -> f (sqrt x))
-- unT (sqrt' 10) (\x -> (x*x))

pyth x y = do
  x2 <- sq' x
  y2 <- sq' y
  z  <- add' x2 y2
  sqrt' z

-- unT (pyth 3 4) id

pyth2 x y =
  sq' x >>= \x2 -> sq' y >>= \y2 -> add' x2 y2 >>= sqrt'

-- unT (pyth2 3 4) id       


----------------
-- factorial
----------------
factorial' :: Integer -> T v Integer
factorial' 0 = return 1
factorial' n = do
  fmn1 <- factorial' (n-1)
  return (n*fmn1)

-- unT (factorial' 5) id

factorial2' :: Integer -> T v Integer
factorial2' 0 = return 1
factorial2' n = factorial2' (n-1) >>= \fmn1 -> return (n*fmn1)

-- unT (factorial2' 5) id

----------------
-- factorial''
-- tail recursive
----------------
factorial'' :: Integer -> T v Integer
factorial'' n = faux' n 1

faux' :: Integer -> Integer -> T v Integer
faux' 0 k = return k
faux' n k = faux' (n-1) (k*n)

-- unT (factorial'' 5) id
