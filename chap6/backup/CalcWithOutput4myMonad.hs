module CalcWithOutput4myMonad where
import Control.Monad
-- Building a Kleisli triple for a program with simple output
-- Type constructor T takes two arguments.

newtype T st a = T {runT :: st -> (a,st)}

-- (=<<) :: (Monad m) => (a -> m a) -> m a -> m b
-- We take m = T st.
extT :: (a -> T st b) -> (T st a -> T st b)
extT f h = 
    T (\st1 -> let (y,st2) = runT h st1
               in  runT (f y) st2 )


-- instance declarations
instance Functor (T st) where
  fmap f ta =
    T (\s ->  let (a,s2) = (runT ta) s
              in  (f a,s2) )
  -- fmap :: (a -> b) -> (T st a -> T st b)

{-
Prop. 
  fmap id == id, or in pointful style,
  fmap id x = x.
Proof.
  fmap id x
    = T (\s -> let (a,s2) = (runT x) s in (a,s2) )
    = T (\s -> runT x s)
    = T (runT x)
    = x
 QED.

Prop. 
  fmap (g.f) == fmap g . fmap f, or in pointful style,
  fmap (g.f) ta == fmap g (fmap f ta).
Proof.
  fmap g (fmap f ta)
    = fmap g (T (\s -> let (a,s2)=runT ta s in (f a,s2) ) )
    = T (\t->let (b,t2)=runT (T (\s->let (a,s2)=runT ta s in (f a,s2))) t
             in (g b,t2) )
    = T (\t->let (b,t2)=(\s -> let (a,s2)=runT ta s in (f a,s2)) t
             in (g b,t2) )
    = T (\t->let (b,t2)=(let (a,s2)=runT ta t in (f a,s2)) in (g b,t2) )
    -- b = f a, t2 == s2
    = T (\t->let (a,s2)=runT ta t
                 (b,t2)=(f a,s2)
             in  (g b,t2) )
    = T (\t->let (a,t2)=runT ta t in ((g.f) a),t2) )
    = fmap (g.f) ta
  QED.

-}

instance Applicative (T st) where
  pure x = T (\s -> (x,s))
  tf <*> ta = 
     T (\s ->
              let (h, s2) = runT tf s
                  (a, s3) = runT ta s2
              in  (h a, s3))
  -- pure :: a -> T st a
  -- (<*>) :: (T st (a -> b)) -> T st a -> T st b

{-
Prop.[identity]
  pure id <*> ta == ta
Proof.
  pure id <*> ta
    = T (\s ->
               let (h,s2) = runT (T (\s->(id,s)) s
                   (a,s3) = runT ta s2
               in  (h a,s3) )
    = T (\s ->
               let (h,s2) = (\s->(id,s)) s
                   (a,s3) = runT ta s2
               in  (h a,s3) )
    = T (\s ->
               let (h,s2) = (id,s)
                   (a,s3) = runT ta s2
               in  (h a,s3) )
    = T (\s -> let (a,s3) = runT ta s
               in  (a,s3) )
    = T (\s -> runT ta s)
    = T (runT ta)
    = ta
  QED.

Prop.[composition]
  pure (.) <*> tu <*> tv <*> tw == tu <*> (tv <*> tw)
  --note--  (.) u v w = (u.v) w == u (v w)
Proof.
  -- LHS
  pure (.)
    = T (\s -> ((.),s) )
    = T (\s -> ((\g -> \f -> \x -> g(f x)),s) )

  pure (.) <*> tu
    = T (\s -> ((\g -> \f -> \x -> g(f x)),s) ) <*> tu
    = T (\ss->let (h,s2) = runT (T(\s->((\g->\f->\x->g(f x)),s) ) ) ss
                  (u,s3) = runT tu s2
              in  (h u,s3) )
    = T (\ss->let (h,s2) = (\s->((\g->\f->\x->g(f x)),s) ss
                  (u,s3) = runT tu s2
              in  (h u,s3) )
    = T (\ss->let (h,s2) = ((\g->\f->\x->g(f x)),ss)
                  (u,s3) = runT tu s2
              in  (h a,s3) )
    = T (\ss->let h=(\g->\f->\x->g(f x))
                  (u,s3) = runT tu ss
              in  (h u,s3) )
    = T (\s-> let (u,s3) = runT tu s
              in  ((\g->\f->\x->g(f x)) u , s3) )
    = T (\s-> let (u,s3) = runT tu s
              in  ((\f->\x->u(f x)) , s3) )
    -- rename local variables
    = T (\s-> let (u,s2) = runT tu s
              in  ((\f->\x->u(f x)) , s2) )

  pure (.) <*> tu <*> tv
    = T (\ss->let (h,s3) = runT (T (\s-> let (u,s2) = runT tu s
                                         in  ((\f->\x->u(f x)) , s2) )
                                ) ss
                  (v,s4) = runT tv s3
              in  (h v,s4)
      )
    = T (\ss->let (h,s3) = (\s-> let (u,s2) = runT tu s
                                 in  ((\f->\x->u(f x)) , s2) )
                           ) ss
                  (v,s4) = runT tv s3
              in  (h v,s4)
      )
    = T (\ss->let (h,s3) = let (u,s2) = runT tu ss
                           in  ((\f->\x->u(f x)) , s2)
                  (v,s4) = runT tv s3
              in  (h v,s4)
      )
    -- notice that s3==s2 and that h can be immediately applied
    = T (\ss->let (u,s2) = runT tu ss
                  (v,s3) = runT tv s2
              in  ((\f->\x->u(f x)) v,s3)
      )
    = T (\ss->let (u,s2) = runT tu ss
                  (v,s3) = runT tv s2
              in  ((\x->u(v x)),s3)
      )

  pure (.) <*> tu <*> tv <*> tw
    = T (\s->let (h,s4)=runT (T (\ss->let (u,s2) = runT tu ss
                                          (v,s3) = runT tv s2
                                      in  ((\x->u(v x)),s3)
                             ) s
                 (w,s5)=runT tw s4
             in  (h w,s5)
      )
    = T (\s->let (h,s4)=(\ss->let (u,s2) = runT tu ss
                                  (v,s3) = runT tv s2
                              in  ((\x->u(v x)),s3)
                        ) s
                 (w,s5)=runT tw s4
             in  (h w,s5)
      )
    = T (\s->let (h,s4)=let (u,s2) = runT tu s
                            (v,s3) = runT tv s2
                        in  ((\x->u(v x)),s3)
                 (w,s5)=runT tw s4
             in  (h w,s5)
      )
    -- notice that s4==s3 and that h can be immediately applied
    = T (\s->let (u,s2) = runT tu s
                 (v,s3) = runT tv s2
                 (w,s4) = runT tw s3
             in  ((\x->u(v x)) w,s4)
      )
    = T (\s->let (u,s2) = runT tu s
                 (v,s3) = runT tv s2
                 (w,s4) = runT tw s3
             in  (u(v w),s4)
      )

  -- RHS
  tv <*> tw
    = T (\s -> let (v,s2)=runT tv s
                   (w,s3)=runT tw s2
               in  (v w,s3)          )

  tu <*> (tv <*> tw)
    = T (\t -> let (u,s4)=runT tu
                   (x,s5)=runT (T (\s -> let (v,s2)=runT tv s
                                             (w,s3)=runT tw s2
                                         in  (v w,s3) )
                               ) s4
               in  (u x,s5)

    -- We rename local variables s4 to s1.
    = T (\t -> let (u,s1)=runT tu t
                   (x,s5)=(\s -> let (v,s2)=runT tv s
                                     (w,s3)=runT tw s2
                                 in  (v w,s3)
                          ) s1
               in  (u x,s5)
        )
    = T (\t -> let (u,s1)=runT tu t
                   (x,s5)=let (v,s2)=runT tv s1
                              (w,s3)=runT tw s2
                          in  (v w,s3)
               in  (u x,s5)
        )
    = T (\t -> let (u,s1)=runT tu t
                   (v,s2)=runT tv s1
                   (w,s3)=runT tw s2
                   (x,s5)=(v w,s3)
               in  (u x,s5)
        )
    = T (\s -> let (u,s1)=runT tu s
                   (v,s2)=runT tv s1
                   (w,s3)=runT tw s2
               in  (u (v w),s3)
        )
    -- rename local variables
    = T (\s -> let (u,s2)=runT tu s
                   (v,s3)=runT tv s2
                   (w,s4)=runT tw s3
               in  (u (v w),s4)
        )
  QED.

Prop.[homomorphism]
  pure f <*> pure x == pure (f x)
Proof.
  pure f <*> pure x
    = T (s -> let (ff,s2) = runT (T (\t -> (f,t)) ) s
                  (xx,s3) = runT (T (\t -> (x,t)) ) s2
              in  (ff xx,s3) )
    -- cancel runT/T pairs
    = T (s -> let (ff,s2) = (\t -> (f,t)) s
                  (xx,s3) = (\t -> (x,t)) s2
              in  (ff xx,s3) )
    -- simple beta reductions
    = T (s -> let (ff,s2) = (f,s)
                  (xx,s3) = (x,s2)
              in  (ff xx,s3) )
    -- trace the chain of bindings
    = T (s -> ((f x),s) )
    = pure (f x)
  QED.


Prop.[iterchange]
  u <*> pure y == pure ($ y) <*> u
Poof.
  -- LHS
  u <*> pure y
    = T (s -> let (uu,s2) = runT u s
                  (yy,s3) = runT (T (\t -> (y,t)) ) s2
              in  (uu yy,s3) )
    -- cancel runT/T pair
    = T (s -> let (uu,s2) = runT u s
                  (yy,s3) = (\t -> (y,t)) s2
              in  (uu yy,s3) )
    -- beta reduction
    = T (s -> let (uu,s2) = runT u s
                  (yy,s3) = (y,s2)
              in  (uu yy,s3) )
    = T (s -> let (uu,s2) = runT u s
                  (yy,s3) = (y,s2)
              in  (uu yy,s3) )
    -- trace the chain of bindings
    = T (s -> let (uu,s2) = runT u s
              in  (uu y,s2) )
    -- prettify local variables
    = T (s -> let (f,s2) = runT u s
              in  (f y,s2) )

  -- RHS
  ($ y) = \f -> f y

  pure ($ y) <*> u
    = pure (\f -> f y) <*> u
    = T (\s -> ((\f->f y),s) ) <*> u
    = T (\t -> let (g,t2) = runT (T (\s -> ((\f->f y),s) )) t
                   (a,t3) = runT u t2
               in  (g a,t3) )
    -- cancel runT/T pair
    = T (\t -> let (g,t2) = (\s -> ((\f->f y),s) ) t
                   (a,t3) = runT u t2
               in  (g a,t3) )
    -- beta reduction
    = T (\t -> let (g,t2) = ((\f->f y),t)
                   (a,t3) = runT u t2
               in  (g a,t3) )
    -- trace the chain of bindings
    = T (\t -> let (a,t3) = runT u t
               in  ((\f->f y) a,t3) )
    -- beta reduction
    = T (\t -> let (a,t3) = runT u t
               in  (a y,t3) )
    -- rewrite local variables
    = T (\s -> let (f,s2) = runT u s
               in  (f y,s2) )
  QED.
-}

instance Monad (T st) where
  return = pure
  h >>= f = extT f h
  --  return :: a -> T st a
  --  (>>=) :: T st a -> (a -> T st b) -> (T st b)

{-
Prop. [left identity]
  return x >>= f  == f x
Proof.
  return x >>= f
    = T (\s -> (x,s)) >>= f
    = extT f $ T (\s -> (x,s))
    = T (\st1 -> let (y,st2) = runT T (\s -> (x,s)) s
                 in  runT (f y) st2 )
    -- cancel runT/T pair
    = T (\st1 -> let (y,st2) = (\s -> (x,s)) st1
                 in  runT (f y) st2 )
    -- beta reduction
    = T (\st1 -> let (y,st2) = (x,st1)
                 in  runT (f y) st2 )
    -- trace the binding chain
    = T (\st1 -> runT (f x) st1 )
    -- eta reduction
    = T (runT (f x))
    -- cancel T/runT pair
    = f x
  QED.

Prop. [right identity]
  m >>= return  == m
Proof.
  m >>= return
    = extT return m
    = T (\st1 -> let (y,st2) = runT m st1
                 in  runT (return y) st2 )
    -- use the definition of return
    = T (\st1 -> let (y,st2) = runT m st1
                 in  runT (T (\s->(y,s) ) st2 )
    -- cancel runT/T pair
    = T (\st1 -> let (y,st2) = runT m st1
                 in  (\s->(y,s) ) st2 )
    -- beta reduction
    = T (\st1 -> let (y,st2) = runT m st1
                 in  (y,st2) )
    -- eliminate redundant binding
    = T (\st1 -> runT m st1)
    -- eta reduction
    = T (runT m)
    -- eliminate T/runT pair
    = m
  QED.



Prop. [associativity]
  (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
Proof.
  -- LHS
  m >>= f
    = extT f m
    = T (\st1 -> let (y,st2) = runT m st1
                 in  runT (f y) st2 )

  (m >>= f) >>= g
    = extT g $ T (\st1 -> let (y,st2) = runT m st1
                          in  (f y) st2 )
    = T (\s1 -> let (p,s2) = runT (T (\st1 -> let (y,st2) = runT m st1
                                              in  runT (f y) st2 )) s1
                in  runT (g p) s2 )
    -- cancel runT/T pair
    = T (\s1 -> let (p,s2) = (\st1 -> let (y,st2) = runT m st1
                                      in  runT (f y) st2 ) s1
                in  runT (g p) s2 )
    -- beta reduction
    = T (\s1 -> let (p,s2) = let (y,st2) = runT m s1
                             in  runT (f y) st2
                in  runT (g p) s2 )
    -- trace the binding chain
    = T (\s1 -> let (y,st2) = runT m s1
                    (z,st3) = runT (f y) st2
                in  runT (g z) st3 )
    -- prettify local variables
    = T (\s1 -> let (v2,s2) = runT m s1
                    (v3,s3) = runT (f v2) s2
                in  runT (g v3) s3 )

  -- RHS
  f x >>= g
    = extT g (f x)
    = T (\st1 -> let (y,st2) = runT (f x) st1
                 in  runT (g y) st2 )

  \x -> f x >>= g
    = \x -> T (\st1 -> let (y,st2) = runT (f x) st1
                       in  runT (g y) st2 )

  m >>= (\x -> f x >>= g)
    = extT (\x -> T (\st1 -> let (y,st2) = runT (f x) st1
                             in  runT (g y) st2 )  )  m
    -- definition of extT
    = T (\s1 -> let (p,s2) = runT m s1
                in  runT ((\x -> T (\st1 -> let (y,st2) = runT (f x) st1
                                            in  runT (g y) st2 ) ) p) s2 )
    -- beta reduction
    = T (\s1 -> let (p,s2) = runT m s1
                in  runT (T (\st1 -> let (y,st2) = runT (f p) st1
                                     in  runT (g y) st2) ) s2 )
    -- cancel runT/T pair
    = T (\s1 -> let (p,s2) = runT m s1
                in  (\st1 -> let (y,st2) = runT (f p) st1
                             in  runT (g y) st2) s2 )
    -- beta reduction
    = T (\s1 -> let (p,s2) = runT m s1
                in  let (y,st2) = runT (f p) s2
                    in  runT (g y) st2 )
    -- trace the binding chain
    = T (\s1 -> let (p,s2) = runT m s1
                    (y,st2) = runT (f p) s2
                in  runT (g y) st2 )
    -- prettify local variables
    = T (\s1 -> let (v2,s2) = runT m s1
                    (v3,s3) = runT (f v2) s2
                in  runT (g v3) s3 )
  QED.
-}

{- We would like to know whether the Applicative instance is compatible
with the Monad instance.

Prop. [Applicative and Monad compatibility]
  mf <*> mx ==  mf >>= \f -> mx >>= \x -> return (f x)
Proof.
  -- LHS
  mf <*> mx
    = T (\s1 -> let (f,s2) = runT mf s1
                    (x,s3) = runT mx s2
                in  (f x,s3) )

  -- RHS - step by step from inside
  return (f x)
    = T (\s -> (f x,s))

  mx >>= \x -> return (f x)
    = extT (\x -> T (\s->(f x,s))) mx
    = T (\t1 -> let (y,t2) = runT mx t1
                in  runT ((\x -> T (\s->(f x,s))) y) t2 )
    -- beta reduction
    = T (\t1 -> let (y,t2) = runT mx t1
                in  runT (T (\s->(f y,s))) t2 )
    -- cancel runT/T pair
    = T (\t1 -> let (y,t2) = runT mx t1
                in  (\s->(f y,s)) t2 )
    -- beta reduction
    = T (\t1 -> let (y,t2) = runT mx t1
                in  (f y,t2) )

  mf >>= \f -> mx >>= \x -> return (f x)
    = mf >>= \f -> T (\t1 -> let (y,t2) = runT mx t1
                             in  (f y,t2) )
    = extT (\f -> T (\t1 -> let (y,t2) = runT mx t1
                            in  (f y,t2) )) mf
    = T (\s1 -> let (v1,s2) = runT mf s1
                in  runT ((\f -> T (\t1 -> let (y,t2) = runT mx t1
                                           in  (f y,t2) )) v1) s2 )
    -- beta reduction
    = T (\s1 -> let (v1,s2) = runT mf s1
                in  runT (T (\t1 -> let (y,t2) = runT mx t1
                                    in  (v1 y,t2) ) ) s2  )
    -- cancel runT/T pair
    = T (\s1 -> let (v1,s2) = runT mf s1
                in (\t1 -> let (y,t2) = runT mx t1
                           in  (v1 y,t2) ) s2 )
    -- beta reduction
    = T (\s1 -> let (v1,s2) = runT mf s1
                    let (y,t2) = runT mx s2
                    in  (v1 y,t2) )
    -- trace the binding chain
    = T (\s1 -> let (v1,s2) = runT mf s1
                    (y,t2) = runT mx s2
                in  (v1 y,t2) )
    -- rename local variables
    = T (\s1 -> let (f,s2) = runT mf s1
                    (x,s3) = runT mx s2
                in  (f x,s3) )
  QED.

-}


--- end of instance declaration

----- test data
f :: Integer -> T String Integer
f x = T (\s -> (x^2, s ++ show x ++ "^2=" ++ show (x^2) ++ ".\n"))

g :: Integer -> T String Float
g x = T (\s -> (sqrt(fromInteger x), s ++  "sqrt of " ++ show x ++ " is " ++ show (sqrt(fromInteger x)) ++ ".\n"))


{-
:kind T String Int
-----> T String Int :: *
:kind T String
-----> T String :: * -> *




(runT $ return 10) ""

let (xTest1, stTest1) = (runT $ extT g $ extT f $ return 10) ""
putStr stTest1

let (xTest2, stTest2) = runT (return 10 >>= f >>= g) ""
putStr stTest2

let (xTest3, stTest3) = runT (do  x <- return 10; y <- f x; g y) ""
putStr stTest3

-}

stTest4 =
     do x <- return 10
        y <- f x
        g y

-- runT stTest4 ""
