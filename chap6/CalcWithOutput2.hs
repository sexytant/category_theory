module CalcWithOutput2 where
import Control.Monad
import CalcWithOutput

-- After building a Kleisli triple
-- for a program with simple output,
-- we declare instances of
-- Functor, Applicative, and Monad

-----------
-- Functor
-----------


instance Functor T where
-- fmap :: (a -> b) -> (T a -> T b)
  fmap f ta =
    T (\s ->  let (a,s2) = (unT ta) s
              in  (f a, s2)
      )


{- suggested tests
-- f1 and f2 are imported from CalcWithOutput

unT (fmap (+1) (f1 10)) "hello\n"
-}


{-
Prop. 
  fmap id = id, or in pointful style,
  fmap id x = x.
Proof.
  fmap id x
    = T (\s -> let (a,s2) = (unT x) s in (a,s2) )
    = T (\s -> unT x s)
    = T (unT x)
    = x
  QED.

Prop. 
  fmap (g.f) = fmap g . fmap f, or in pointful style,
  fmap (g.f) ta = fmap g (fmap f ta).
Proof.
  fmap g (fmap f ta)
    = fmap g (T (\s -> let (a,s2)=unT ta s in (f a,s2) ) )
    = T (\t->let (b,t2)=unT (T (\s->let (a,s2)=unT ta s in (f a,s2))) t
             in (g b,t2) )
    = T (\t->let (b,t2)=(\s -> let (a,s2)=unT ta s in (f a,s2)) t
             in (g b,t2) )
    = T (\t->let (b,t2)=(let (a,s2)=unT ta t in (f a,s2)) in (g b,t2) )
    -- b = f a, t2 == s2
    = T (\t->let (a,s2)=unT ta t
                 (b,t2)=(f a,s2)
             in  (g b,t2) )
    = T (\t->let (a,t2)=unT ta t in ((g.f) a),t2) )
    = fmap (g.f) ta
  QED.
-}

----------------
-- Applicative
----------------

instance Applicative T where
  pure x = T (\s -> (x,s))
  tf <*> ta = 
     T (\s ->
              let (h, s2) = unT tf s
                  (a, s3) = unT ta s2
              in  (h a, s3))
  -- pure :: a -> T a
  -- (<*>) :: (T (a -> b)) -> T a -> T b


-------------
-- Test data
-------------

appTest1 :: T (Integer -> Integer)
appTest1 =
    T (\s -> ((\x -> x * 3),
              s ++ "multiplied by 3 <= ")
      )

{- suggested tests
:t appTest1 <*> f1 10
:t unT $ appTest1 <*> f1 10
unT (appTest1 <*> f1 10) ""
-}


{-
Prop.[identity]
  pure id <*> ta = ta
Proof.
  pure id <*> ta
    = T (\s ->
               let (h,s2) = unT (T (\s->(id,s)) s
                   (a,s3) = unT ta s2
               in  (h a,s3) )
    = T (\s ->
               let (h,s2) = (\s->(id,s)) s
                   (a,s3) = unT ta s2
               in  (h a,s3) )
    = T (\s ->
               let (h,s2) = (id,s)
                   (a,s3) = unT ta s2
               in  (h a,s3) )
    = T (\s -> let (a,s3) = unT ta s
               in  (a,s3) )
    = T (\s -> unT ta s)
    = T (unT ta)
    = ta
  QED.

Prop.[composition]
  pure (.) <*> tu <*> tv <*> tw = tu <*> (tv <*> tw)
  --note--  (.) u v w = (u.v) w = u (v w)
Proof.
  -- LHS
  pure (.)
    = T (\s -> ((.),s) )
    = T (\s -> ((\g -> \f -> \x -> g(f x)),s) )

  pure (.) <*> tu
    = T (\s -> ((\g -> \f -> \x -> g(f x)),s) ) <*> tu
    = T (\ss->let (h,s2) = unT (T(\s->((\g->\f->\x->g(f x)),s) ) ) ss
                  (u,s3) = unT tu s2
              in  (h u,s3) )
    = T (\ss->let (h,s2) = (\s->((\g->\f->\x->g(f x)),s) ss
                  (u,s3) = unT tu s2
              in  (h u,s3) )
    = T (\ss->let (h,s2) = ((\g->\f->\x->g(f x)),ss)
                  (u,s3) = unT tu s2
              in  (h a,s3) )
    = T (\ss->let h=(\g->\f->\x->g(f x))
                  (u,s3) = unT tu ss
              in  (h u,s3) )
    = T (\s-> let (u,s3) = unT tu s
              in  ((\g->\f->\x->g(f x)) u , s3) )
    = T (\s-> let (u,s3) = unT tu s
              in  ((\f->\x->u(f x)) , s3) )
    -- rename local variables
    = T (\s-> let (u,s2) = unT tu s
              in  ((\f->\x->u(f x)) , s2) )

  pure (.) <*> tu <*> tv
    = T (\ss->let (h,s3) = unT (T (\s-> let (u,s2) = unT tu s
                                         in  ((\f->\x->u(f x)) , s2) )
                                ) ss
                  (v,s4) = unT tv s3
              in  (h v,s4)
      )
    = T (\ss->let (h,s3) = (\s-> let (u,s2) = unT tu s
                                 in  ((\f->\x->u(f x)) , s2) )
                           ) ss
                  (v,s4) = unT tv s3
              in  (h v,s4)
      )
    = T (\ss->let (h,s3) = let (u,s2) = unT tu ss
                           in  ((\f->\x->u(f x)) , s2)
                  (v,s4) = unT tv s3
              in  (h v,s4)
      )
    -- notice that s3==s2 and that h can be immediately applied
    = T (\ss->let (u,s2) = unT tu ss
                  (v,s3) = unT tv s2
              in  ((\f->\x->u(f x)) v,s3)
      )
    = T (\ss->let (u,s2) = unT tu ss
                  (v,s3) = unT tv s2
              in  ((\x->u(v x)),s3)
      )

  pure (.) <*> tu <*> tv <*> tw
    = T (\s->let (h,s4)=unT (T (\ss->let (u,s2) = unT tu ss
                                          (v,s3) = unT tv s2
                                     in  ((\x->u(v x)),s3)
                             ) s
                 (w,s5)=unT tw s4
             in  (h w,s5)
      )
    = T (\s->let (h,s4)=(\ss->let (u,s2) = unT tu ss
                                  (v,s3) = unT tv s2
                              in  ((\x->u(v x)),s3)
                        ) s
                 (w,s5)=unT tw s4
             in  (h w,s5)
      )
    = T (\s->let (h,s4)=let (u,s2) = unT tu s
                            (v,s3) = unT tv s2
                        in  ((\x->u(v x)),s3)
                 (w,s5)=unT tw s4
             in  (h w,s5)
      )
    -- notice that s4==s3 and that h can be immediately applied
    = T (\s->let (u,s2) = unT tu s
                 (v,s3) = unT tv s2
                 (w,s4) = unT tw s3
             in  ((\x->u(v x)) w,s4)
      )
    = T (\s->let (u,s2) = unT tu s
                 (v,s3) = unT tv s2
                 (w,s4) = unT tw s3
             in  (u(v w),s4)
      )

  -- RHS
  tv <*> tw
    = T (\s -> let (v,s2)=unT tv s
                   (w,s3)=unT tw s2
               in  (v w,s3)          )

  tu <*> (tv <*> tw)
    = T (\t -> let (u,s4)=unT tu
                   (x,s5)=unT (T (\s -> let (v,s2)=unT tv s
                                             (w,s3)=unT tw s2
                                         in  (v w,s3) )
                               ) s4
               in  (u x,s5)

    -- We rename local variables s4 to s1.
    = T (\t -> let (u,s1)=unT tu t
                   (x,s5)=(\s -> let (v,s2)=unT tv s
                                     (w,s3)=unT tw s2
                                 in  (v w,s3)
                          ) s1
               in  (u x,s5)
        )
    = T (\t -> let (u,s1)=unT tu t
                   (x,s5)=let (v,s2)=unT tv s1
                              (w,s3)=unT tw s2
                          in  (v w,s3)
               in  (u x,s5)
        )
    = T (\t -> let (u,s1)=unT tu t
                   (v,s2)=unT tv s1
                   (w,s3)=unT tw s2
                   (x,s5)=(v w,s3)
               in  (u x,s5)
        )
    = T (\s -> let (u,s1)=unT tu s
                   (v,s2)=unT tv s1
                   (w,s3)=unT tw s2
               in  (u (v w),s3)
        )
    -- rename local variables
    = T (\s -> let (u,s2)=unT tu s
                   (v,s3)=unT tv s2
                   (w,s4)=unT tw s3
               in  (u (v w),s4)
        )
  QED.

Prop.[homomorphism]
  pure f <*> pure x = pure (f x)
Proof.
  pure f <*> pure x
    = T (s -> let (ff,s2) = unT (T (\t -> (f,t)) ) s
                  (xx,s3) = unT (T (\t -> (x,t)) ) s2
              in  (ff xx,s3) )
    -- cancel unT/T pairs
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
  u <*> pure y = pure ($ y) <*> u
Poof.
  -- LHS
  u <*> pure y
    = T (s -> let (uu,s2) = unT u s
                  (yy,s3) = unT (T (\t -> (y,t)) ) s2
              in  (uu yy,s3) )
    -- cancel unT/T pair
    = T (s -> let (uu,s2) = unT u s
                  (yy,s3) = (\t -> (y,t)) s2
              in  (uu yy,s3) )
    -- beta reduction
    = T (s -> let (uu,s2) = unT u s
                  (yy,s3) = (y,s2)
              in  (uu yy,s3) )
    -- trace the chain of bindings
    = T (s -> let (uu,s2) = unT u s
              in  (uu y,s2) )
    -- prettify local variables
    = T (s -> let (f,s2) = unT u s
              in  (f y,s2) )

  -- RHS
  ($ y) = \f -> f y

  pure ($ y) <*> u
    = pure (\f -> f y) <*> u
    = T (\s -> ((\f->f y),s) ) <*> u
    = T (\t -> let (g,t2) = unT (T (\s -> ((\f->f y),s) )) t
                   (a,t3) = unT u t2
               in  (g a,t3) )
    -- cancel unT/T pair
    = T (\t -> let (g,t2) = (\s -> ((\f->f y),s) ) t
                   (a,t3) = unT u t2
               in  (g a,t3) )
    -- beta reduction
    = T (\t -> let (g,t2) = ((\f->f y),t)
                   (a,t3) = unT u t2
               in  (g a,t3) )
    -- trace the chain of bindings
    = T (\t -> let (a,t3) = unT u t
               in  ((\f->f y) a,t3) )
    -- beta reduction
    = T (\t -> let (a,t3) = unT u t
               in  (a y,t3) )
    -- rewrite local variables
    = T (\s -> let (f,s2) = unT u s
               in  (f y,s2) )
  QED.
-}

----------
-- Monad
----------

instance Monad T where
  return = pure
  h >>= f = sharp f h
  --  return :: a -> T a
  --  (>>=) :: T a -> (a -> T b) -> (T b)

{- suggested tests
(unT $ return 10) ""

let (xTest1, stTest1) = (unT $ sharp f2 $ sharp f1 $ return 10) ""
putStr stTest1

let (xTest2, stTest2) = unT (return 10 >>= f1 >>= f2) ""
putStr stTest2

let (xTest3, stTest3) = unT (do  x <- return 10; y <- f1 x; f2 y) ""
putStr stTest3
-}


{-
Prop. [left identity]
  return x >>= f  = f x
Proof.
  return x >>= f
    = T (\s -> (x,s)) >>= f
    = sharp f $ T (\s -> (x,s))
    = T (\st1 -> let (y,st2) = unT T (\s -> (x,s)) s
                 in  unT (f y) st2 )
    -- cancel unT/T pair
    = T (\st1 -> let (y,st2) = (\s -> (x,s)) st1
                 in  unT (f y) st2 )
    -- beta reduction
    = T (\st1 -> let (y,st2) = (x,st1)
                 in  unT (f y) st2 )
    -- trace the binding chain
    = T (\st1 -> unT (f x) st1 )
    -- eta conversion
    = T (unT (f x))
    -- cancel T/unT pair
    = f x
  QED.

Prop. [right identity]
  m >>= return  = m
Proof.
  m >>= return
    = sharp return m
    = T (\st1 -> let (y,st2) = unT m st1
                 in  unT (return y) st2 )
    -- use the definition of return
    = T (\st1 -> let (y,st2) = unT m st1
                 in  unT (T (\s->(y,s) ) st2 )
    -- cancel unT/T pair
    = T (\st1 -> let (y,st2) = unT m st1
                 in  (\s->(y,s) ) st2 )
    -- beta reduction
    = T (\st1 -> let (y,st2) = unT m st1
                 in  (y,st2) )
    -- eliminate redundant binding
    = T (\st1 -> unT m st1)
    -- eta conversion
    = T (unT m)
    -- eliminate T/unT pair
    = m
  QED.



Prop. [associativity]
  (m >>= f) >>= g  =  m >>= (\x -> f x >>= g)
Proof.
  -- LHS
  m >>= f
    = sharp f m
    = T (\st1 -> let (y,st2) = unT m st1
                 in  unT (f y) st2 )

  (m >>= f) >>= g
    = sharp g $ T (\st1 -> let (y,st2) = unT m st1
                           in  (f y) st2 )
    = T (\s1 -> let (p,s2) = unT (T (\st1 -> let (y,st2) = unT m st1
                                             in  unT (f y) st2 )) s1
                in  unT (g p) s2 )
    -- cancel unT/T pair
    = T (\s1 -> let (p,s2) = (\st1 -> let (y,st2) = unT m st1
                                      in  unT (f y) st2 ) s1
                in  unT (g p) s2 )
    -- beta reduction
    = T (\s1 -> let (p,s2) = let (y,st2) = unT m s1
                             in  unT (f y) st2
                in  unT (g p) s2 )
    -- trace the binding chain
    = T (\s1 -> let (y,st2) = unT m s1
                    (z,st3) = unT (f y) st2
                in  unT (g z) st3 )
    -- prettify local variables
    = T (\s1 -> let (v2,s2) = unT m s1
                    (v3,s3) = unT (f v2) s2
                in  unT (g v3) s3 )

  -- RHS
  f x >>= g
    = sharp g (f x)
    = T (\st1 -> let (y,st2) = unT (f x) st1
                 in  unT (g y) st2 )

  \x -> f x >>= g
    = \x -> T (\st1 -> let (y,st2) = unT (f x) st1
                       in  unT (g y) st2 )

  m >>= (\x -> f x >>= g)
    = sharp (\x -> T (\st1 -> let (y,st2) = unT (f x) st1
                              in  unT (g y) st2 )  )  m
    -- definition of sharp
    = T (\s1 -> let (p,s2) = unT m s1
                in  unT ((\x -> T (\st1 -> let (y,st2) = unT (f x) st1
                                           in  unT (g y) st2 ) )
                         p) 
                    s2
    -- beta reduction
    = T (\s1 -> let (p,s2) = unT m s1
                in  unT (T (\st1 -> let (y,st2) = unT (f p) st1
                                    in  unT (g y) st2) )
                    s2
    -- cancel unT/T pair
    = T (\s1 -> let (p,s2) = unT m s1
                in  (\st1 -> let (y,st2) = unT (f p) st1
                             in  unT (g y) st2)
                    s2
    -- beta reduction
    = T (\s1 -> let (p,s2) = unT m s1
                in  let (y,st2) = unT (f p) s2
                    in  unT (g y) st2 )
    -- trace the binding chain
    = T (\s1 -> let (p,s2) = unT m s1
                    (y,st2) = unT (f p) s2
                in  unT (g y) st2 )
    -- prettify local variables
    = T (\s1 -> let (v2,s2) = unT m s1
                    (v3,s3) = unT (f v2) s2
                in  unT (g v3) s3 )
  QED.
-}

{- We would like to know whether the Applicative instance is compatible
with the Monad instance.

Prop. [Applicative and Monad compatibility]
  mf <*> mx =  mf >>= \f -> mx >>= \x -> return (f x)
Proof.
  -- LHS
  mf <*> mx
    = T (\s1 -> let (f,s2) = unT mf s1
                    (x,s3) = unT mx s2
                in  (f x,s3) )

  -- RHS - step by step from inside
  return (f x)
    = T (\s -> (f x,s))

  mx >>= \x -> return (f x)
    = sharp (\x -> T (\s->(f x,s))) mx
    = T (\t1 -> let (y,t2) = unT mx t1
                in  unT ((\x -> T (\s->(f x,s)))
                         y)
                    t2 )
    -- beta reduction
    = T (\t1 -> let (y,t2) = unT mx t1
                in  unT (T (\s->(f y,s)))
                    t2 )
    -- cancel unT/T pair
    = T (\t1 -> let (y,t2) = unT mx t1
                in  (\s->(f y,s)) t2 )
    -- beta reduction
    = T (\t1 -> let (y,t2) = unT mx t1
                in  (f y,t2) )

  mf >>= \f -> mx >>= \x -> return (f x)
    = mf >>= \f -> T (\t1 -> let (y,t2) = unT mx t1
                             in  (f y,t2) )
    = sharp (\f -> T (\t1 -> let (y,t2) = unT mx t1
                             in  (f y,t2) )) mf
    = T (\s1 -> let (v1,s2) = unT mf s1
                in  unT ((\f -> T (\t1 -> let (y,t2) = unT mx t1
                                          in  (f y,t2) ))
                         v1)
                    s2 )
    -- beta reduction
    = T (\s1 -> let (v1,s2) = unT mf s1
                in  unT (T (\t1 -> let (y,t2) = unT mx t1
                                   in  (v1 y,t2) ) )
                    s2 )
    -- cancel unT/T pair
    = T (\s1 -> let (v1,s2) = unT mf s1
                in (\t1 -> let (y,t2) = unT mx t1
                           in  (v1 y,t2) )
                   s2 )
    -- beta reduction
    = T (\s1 -> let (v1,s2) = unT mf s1
                    let (y,t2) = unT mx s2
                    in  (v1 y,t2) )
    -- trace the binding chain
    = T (\s1 -> let (v1,s2) = unT mf s1
                    (y,t2) = unT mx s2
                in  (v1 y,t2) )
    -- rename local variables
    = T (\s1 -> let (f,s2) = unT mf s1
                    (x,s3) = unT mx s2
                in  (f x,s3) )
  QED.
-}
