module Continuation2myMonad where
import Control.Monad
---- In this version, type constructor T takes two arguments.
---- cf. L16_FunctorApplicativeMonad.ppt

newtype T v a = T {runT :: (a -> v) -> v}
unitT x = T (\a' -> a' x)

-- f :: a -> T v b
-- m :: T v a
-- b' :: b -> v
extT :: (a -> T v b) -> (T v a -> T v b)
extT f m = 
    T (\b' -> runT m (\a -> runT (f a) b'))


---- instance declarations

-- T v a is a bifunctor.
-- However, we declare a Fuctor instance only for (T v).

instance Functor (T v) where
  fmap f h =
    T (\b' -> runT h (b'.f))

  -- where
  -- fmap  :: (a -> b) -> (T v a -> T v b)
  -- f     ::  a -> b
  -- runT h :: (a -> v) -> v
  -- b' ::  b -> v
  -- result::  v

{- We have to prove the following functor rules.
Prop.
  fmap id == id, or in pointful style, 
  fmap id h == h.
Proof.
  T (\b' -> runT h (b'.id)) 
    = T (\b' -> runT h b') 
    -- alpha reduction
    = T $ runT h 
    -- cancel T/runT pair
    = h
  QED.

Prop.
  fmap (g.f) == fmap g . fmap f, or in pointful style
  fmap (g.f) h == fmap g (fmap f h).
Proof.
  Let
    f :: a -> b
    g :: b -> c
    runT h :: (a -> v) -> v
    c' :: (c -> v)
    b' :: (b -> v)

  -- LHS
  fmap (g.f) h = T (\c' -> runT h (c'.g.f))

  -- RHS
  fmap g (fmap f h) 
    = fmap g (T (\b' -> runT h (b'.f))
    = T (\c' -> runT (T (\b' -> runT h (b'.f))) (c'.g))
    -- cancel runT/T pair
    = T (\c' -> (\b' -> runT h (b'.f)) (c'.g))
    -- beta reduction
    = T (\c' -> runT h (c'.g.f))
  QED.
-}

-- Note that T itself is not an Applicative, but (T v) is.

instance (Applicative (T v)) where
  pure = unitT
  -- pure :: a -> T v a
  tf <*> ta = 
     T (\b' -> runT tf $ \f -> runT ta (b'.f))
  -- where
  -- (<*>)   :: (T v (a -> b)) -> T v a -> T v b
  -- runT tf :: ((a->b)->v) -> v
  -- runT ta :: (  a   ->v) -> v
  -- b'      ::    b   ->v
  -- f       ::    a   ->b
  -- b'.f    ::    a   ->v
{- We have to prove the following Applicative rules.
Prop.[identity]
  pure id <*> ta == ta
Proof.
  pure id <*> ta
    = unitT id <*> ta
    = T (\aa' -> aa' id) <*> ta
    = T (\b' -> runT (T (\aa' -> aa' id)) $ \f -> runT ta (b'.f))
    -- cancel runT/T pair
    = T (\b' -> (\aa' -> aa' id) $ \f -> runT ta (b'.f))
    -- beta reduction
    = T (\b' -> (\f -> runT ta (b'.f)) id)
    -- beta reduction
    = T (\b' -> runT ta (b'.id))
    = T (\b' -> runT ta b')
    -- eta reduction
    = T (runT ta)
    -- cancel T/runT pair
    = ta

  -- where
  -- runT ta :: ((a->b)->v) -> v
  -- aa'     ::  (a->a)->v
  QED.

Prop.[composition]
  pure (.) <*> tu <*> tv <*> tw == tu <*> (tv <*> tw)
  --note--  (.) u v w = (u.v) w == u (v w)
Proof.
  -- (.)     :: (b->c) -> (a->b) -> (a->c)
  (.) = \g -> \f -> \x -> g (f x)

  -- bcabac' :: ((b->c) -> (a->b) -> (a->c)) -> v
  -- pure (.) :: T v ((b->c) -> (a->b) -> (a->c)) 
  pure (.) = T (\bcabac' -> bcabac' (.))

  -- tu              :: T v (b->c)
  -- runT tu         :: ((b->c)->v) -> v
  -- pure (.) <*> tu :: T v ((a->b) -> (a->c))
  -- abac'           :: ((a->b) -> (a->c)) -> v
  -- abac'.z         :: (b->c)->v
  -- z               :: (b->c) -> ((a->b) -> (a->c))
  pure (.) <*> tu
    = unitT (.) <*> tu
    = T ( \bcabac' -> bcabac' (.) ) <*> tu
    = T ( \abac' -> runT (T ( \bcabac' -> bcabac' (.) )) $ \z -> runT tu $ abac'.z )
    = T ( \abac' -> (\bcabac' -> bcabac' (.)) $ \z -> runT tu $ abac'.z )
    = T ( \abac' -> (\z -> runT tu $ abac'.z) (.) )
    = T ( \abac' -> runT tu $ abac'.(.) )
    = T ( \abac' -> runT tu $ abac'.(\u -> \v -> \x -> u (v x)) )
    = T ( \abac' -> runT tu $ \u -> abac' $ \v -> \x -> u (v x) )

  -- tv              :: T v (a->b)
  -- runT tv         :: ((a->b)->v) -> v
  -- pure (.) <*> tu <*> tv :: T v (a->c)
  -- g               :: b->c

  pure (.) <*> tu <*> tv
    = T ( \abac' -> runT tu $ \u -> abac' $ \v -> \x -> u (v x) ) <*> tv
    = T ( \ac' -> \abac' -> runT tu $ \u -> abac' $ \v -> \x -> u (v x)
                      $ \g -> runT tv (ac'.g)  )
    = T ( \ac' -> runT tu $ \u -> (\g -> runT tv (ac'.g)) $ \v -> \x -> u (v x) )
    = T ( \ac' -> runT tu $ \u -> runT tv $ ac'.(\v -> \x -> u (v x)) )

  -- tw                            :: T v a
  -- pure (.) <*> tu <*> tv <*> tw :: T v c
  --
  pure (.) <*> tu <*> tv <*> tw
    = T (c' -> \ac' -> runT tu $ \u -> runT tv $ ac'.(\v -> \x -> u (v x)) )
                    $ f -> runT tw (c'.f)
    = T (c' -> runT tu $ \u -> runT tv $ (f -> runT tw (c'.f)).(\v -> \x -> u (v x)) )
    = T (c' -> runT tu $ \u -> runT tv $ \v -> runT tw $ c'.(\x -> u (v x))  )
    = T (c' -> runT tu $ \u -> runT tv $ \v -> runT tw $ \x -> c' $ u (v x)  )
    = T (c' -> runT tu $ \u -> runT tv $ \v -> runT tw $ c'.u.v)

  -- tv        :: T v (a->b)
  -- runT tv   :: ((a->b)->v) -> v
  -- tw        :: T v a
  -- runT tw   :: (a->v) -> v
  -- tv <*> tw :: T v b
  -- b'        :: b -> v
  -- v         :: a -> b
  tv <*> tw
    = T (\b' -> runT tv $ \v -> runT tw (b'.v))

  -- tu <*> (tv <*> tw) :: T v c
  -- c'                 :: c -> v
  -- b'                 :: b -> v
  -- u                  :: b -> c
  tu <*> (tv <*> tw)
    = T (\c' -> runT tu $ \u -> runT (T (\b' -> runT tv 
                                 $ \v -> runT tw (b'.v))) (c'.u) )
    = T (\c' -> runT tu $ \u -> (\b' -> runT tv $ \v -> runT tw (b'.v)) (c'.u) ) )
    = T (\c' -> runT tu $ \u -> runT tv $ \v -> runT tw $ c'.u.v )
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
    = T (b' -> runT (T (\ab'->ab' f)) $ \g -> runT (T (\a'->a' x)) (b'.g) )
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
    = T (\b' -> runT u $ \g -> runT (T (\a'->a' y)) (b'.g) )
    = T (\b' -> runT u $ \g -> (\a'->a' y) (b'.g) )
    = T (\b' -> runT u $ \g -> (b'.g) y )
    = T (\b' -> runT u $ \g -> b' (g y) )

  -- $ y          :: (a->v) -> v
  pure ($ y) <*> u
    = pure (\f -> f y) <*> u
    = T (\g -> g (\f -> f y)) <*> u
    = T (\b' -> runT (T (\g -> g (\f -> f y))) $ \h -> runT u (b'.h) )
    = T (\b' -> (\g -> g (\f -> f y)) $ \h -> runT u (b'.h) )
    = T (\b' -> (\h -> runT u (b'.h)) (\f -> f y) )
    = T (\b' -> runT u $ b'.(\f -> f y) )
    = T (\b' -> runT u $ \f -> b' (f y) )
  QED.

-}


-- Note that T itself is not a Monad, but (T v) is.
instance (Monad (T v)) where
  return = pure
  h >>= f == extT f h
  --  return :: a -> T v a
  --  (>>=) :: T v a -> (a -> T v b) -> (T v b)

{-
Prop. [left identity]
  return x >>= f  == f x
Proof.
  f  :: a -> T v b
  b' :: b -> v
  return x >>= f
    = T (\a' -> a' x) >>= f
    = extT f $ T (\a' -> a' x)
    = T (\b' -> runT (T (\a' -> a' x)) $ \a -> runT (f a) b' )
    = T (\b' -> (\a' -> a' x) $ \a -> runT (f a) b' )
    = T (\b' -> runT (f x) b')
    = T $ runT (f x)
    = f x
  QED.

Prop. [right identity]
  m >>= return  == m
Proof.
  m      :: T v a
  return :: a -> T v a
  m >>= return
    = extT return m
    = T (\a' -> runT m $ \a -> runT (return a) a' )
    = T (\a' -> runT m $ \a -> runT (T (\f -> f a)) a' )
    = T (\a' -> runT m $ \a -> (\f -> f a) a' )
    = T (\a' -> runT m $ \a -> a' a )
    = T (\a' -> runT m a')
    = T (runT m)
    = m
  QED.

Prop. [associativity]
  (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
Proof.
  m   :: T v a
  f   :: a -> T v b
  g   :: b -> T v c
  (m >>= f) >>= g
    = (extT f m) >>= g
    = extT g (extT f m)
    = extT g $ T (\b' -> runT m $ \a -> runT (f a) b')
    = T (\c' -> (runT (T (\b' -> runT m $ \a -> runT (f a) b')))
                  \b -> runT (g b) c' )
    = T (\c' -> (\b' -> runT m $ \a -> runT (f a) b')
                  \b -> runT (g b) c' )
    = T (\c' -> runT m $ \a -> runT (f a) $ \b -> runT (g b) c' )

  -- x   :: a
  -- f x :: T v b
  -- g   :: b -> T v c
  f x >>= g
    = extT g (f x)
    = T (\c' -> runT (f x) (\b -> runT (g b) c') )

  -- (\x -> f x >>= g) :: a -> T v c
  -- m                 :: T v a
  -- c', d'            :: c -> v
  m >>= (\x -> f x >>= g)
    = extT (\x -> T (\c' -> runT (f x) (\b -> runT (g b) c') )) m
    = T (\d' -> runT m $ 
                 \a -> runT ( T (\c' -> runT (f a) (\b -> runT (g b) c')))) d')
    = T (\d' -> runT m $ 
                 \a -> (\c' -> runT (f a) (\b -> runT (g b) c')) d')
                       -----------------------------------------
    = T (\d' -> runT m $
                 \a -> (runT (f a) (\b -> runT (g b) d')) )
    = T (\d' -> runT m $ \a -> runT (f a) $ \b -> runT (g b) d' )
  QED.

-}



--- for tests

f :: Int -> T Int Int
f x = T (\c -> c (x^2))


g2 :: String -> T Int String
g2 x = T (\c -> c (x++x))

test9 =
  do x <- return "hello"
     y <- g2 x
     g2 y

-- runT test9 length

{-
runT (f 3) (+1) ---> 10
runT (return 3 >>= f) (+1)
runT (extT f (unitT 3)) (+1)


runT (extT f $ extT f $ unitT 3) id   -----> 81
runT (return 3 >>= f >>= f) id

runT (g2 "hello") length -----> 10
runT (extT g2 (unitT "hello") ) length
runT (return "hello" >>= g2) length

(runT $ extT g2 $ extT g2 $ unitT "hello") length -------> 20
runT (return "hello" >>= g2 >>= g2) length
-}


-------------
-- Pythagoras
-------------

sq' :: (Num a) => a -> T v a
sq' x = return (x*x)
-- sq' x = T (\f -> f (x*x))
-- runT (sq' 3) id ----------> 9

add' :: (Num a) => a -> a -> T v a
add' x y = return (x+y)
-- add' x y = T (\f -> f (x+y))
-- runT (add' 11 19) id ----------> 30

sqrt' :: (Floating a) => a -> T v a
sqrt' x = return (sqrt x)
-- sqrt' x = T (\f -> f (sqrt x))
-- runT (sqrt' 10) (\x -> (x*x))

pyth x y = do
  x2 <- sq' x
  y2 <- sq' y
  z  <- add' x2 y2
  sqrt' z

-- runT (pyth 3 4) id

pyth2 x y =
  sq' x >>= \x2 -> sq' y >>= \y2 -> add' x2 y2 >>= sqrt'

-- runT (pyth2 3 4) id       


----------------
-- factorial
----------------
factorial' :: Integer -> T v Integer
factorial' 0 = return 1
factorial' n = do
  fmn1 <- factorial' (n-1)
  return (n*fmn1)

-- runT (factorial' 5) id

factorial2' :: Integer -> T v Integer
factorial2' 0 = return 1
factorial2' n = factorial2' (n-1) >>= \fmn1 -> return (n*fmn1)

-- runT (factorial2' 5) id

----------------
-- factorial''
-- tail recursive
----------------
factorial'' :: Integer -> T v Integer
factorial'' n = faux' n 1

faux' :: Integer -> Integer -> T v Integer
faux' 0 k = return k
faux' n k = faux' (n-1) (k*n)

-- runT (factorial'' 5) id
