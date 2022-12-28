module ContraFunctor where
-- ContraFunctor.hs
-- 標準の Data.Functor.Functor では反変関手を包括できないので Contravariant とい
-- う別の種類の関手が定義されてる。でも本当はそんな余分な定義は要らないんだ。
-- 全部 Functor に一元化できる。そう、Control.Category ならね。

{-# LANGUAGE MultiParamTypeClasses, TypeOperators, TypeSynonymInstances,
             InstanceSigs, FlexibleInstances, ScopedTypeVariables,
             TypeFamilies, FlexibleContexts #-}
import Control.Category
import Prelude hiding ((.), id, Functor(..))

import Control.Arrow (Arrow(..))

-- [圏論セットアップ]

-- 圏は、hom を決めると他のもの (対象の集合とか、射の型とか) が芋づる式に決まる
-- ので hom を圏そのものと見なして良い。これが Control.Category の定義。
{-
class Category hom where
    id :: hom a a
    (.) :: hom b c -> hom a b -> hom a c
-}

-- 例えば Hask では hom a b は単に (a -> b) の事。インスタンスは
-- Control.Category で宣言されてる。
type Hask = (->)

-- Hom が圏そのものとみなすと、Hask^op は単に <- という型構築子になる。newtype
-- 挟まないとインスタンスとか書けないのがちょっと残念。Ticket #785 参照。
-- http://hackage.haskell.org/trac/ghc/ticket/785
newtype (:<-) a b = HaskOp (b -> a)
type HaskOp = (:<-)
instance Category HaskOp where
    id = HaskOp id
    HaskOp f . HaskOp g = HaskOp (g . f)


-- [関手の定義]

-- 圏 hom1 から hom2 への関手。
class (Category hom1, Category hom2) => Functor f hom1 hom2 where
    fmap :: hom1 a b -> hom2 (f a) (f b)


-- [関手の例]

-- 共変関手の例: みんな大好きリスト関手
instance Functor [] Hask Hask where
    fmap :: (a -> b) -> ([a] -> [b])
    fmap = map

-- 反変関手の例: 反変 hom 関手
instance Functor ((:<-) ans) HaskOp Hask where
    fmap :: (a :<- b) -> ((ans :<- a) -> (ans :<- b))
    fmap = (.)
    -- 注: ↑は↓に同じ
    -- fmap (HaskOp f) (HaskOp g) = HaskOp $ g.f


-- [双関手]

-- 双関手は普通に定義すると Functor とは別物になって、例えばこんなんなる。
class (Category hom1, Category hom2, Category hom3) =>
    Bifunctor f hom1 hom2 hom3 where
    bimap :: hom1 a b -> hom2 c d -> hom3 (f a c) (f b d)

-- 共変双関手の例:
instance Bifunctor (,) Hask Hask Hask where
    bimap :: (a -> b) -> (c -> d) -> ((a,c) -> (b,d))
    bimap f g (x,y) = (f x, g y)
instance Bifunctor Either Hask Hask Hask where
    bimap :: (a -> b) -> (c -> d) -> (Either a c -> Either b d)
    bimap f g = either (Left . f) (Right . g)

-- 左引数だけ反変な関手 (profunctor) の例:
instance Bifunctor (->) HaskOp Hask Hask where
    bimap :: (a :<- b) -> (c -> d) -> ((a -> c) -> (b -> d))
    bimap (HaskOp f) g = (g.) . (.f)

-- Arrow は profunctor の特殊な場合。
instance Arrow arr => Bifunctor arr HaskOp Hask Hask where
    bimap :: (a :<- b) -> (c -> d) -> (arr a c -> arr b d)
    bimap (HaskOp f) g = (arr g <<<) . (<<< arr f)

-- で、この双関手というのはアンカリーすると普通の関手の一種と見なすことができる
-- ので厳密には Bifunctor という別の定義は必要ない。…けど、以下に見られるように
-- newtype やら型族の嵐で色々めんどくさくなるから、実際的には別定義にした方が便
-- 利ぽ。



-- [直積圏]

-- C × D の直積圏では hom は hom (a,b) (c,d) のように引数が対の時にしか定義され
-- てない上、(a -> c) と (b -> d) にバラさないといけないので型族が必要になる。

type family Fst a
type instance Fst (a,b) = a
type family Snd a
type instance Snd (a,b) = b

newtype (:×) hom1 hom2 a b =
    Product (hom1 (Fst a) (Fst b), hom2 (Snd a) (Snd b))

instance (Category hom1, Category hom2) => Category (hom1 :× hom2) where
    id :: (hom1 :× hom2) a a
    id = Product (id, id)
    Product (f1, f2) . Product (g1, g2) = Product (f1.g1, f2.g2)

-- 対角関手
newtype Δ a = Δ (a,a)
type instance Fst (Δ a) = a
type instance Snd (Δ a) = a

instance Category hom => Functor Δ hom (hom :× hom) where
    fmap :: hom a b -> (hom :× hom) (Δ a) (Δ b)
    fmap f = Product (f,f)

-- …とまあ、型宣言はすんなり通るんだけど、型族のせいで Product (f,g) という項か
-- ら :× の最後の二つの型引数を一意に推論できない (例えば Δ 型なのか (,) 型な
-- のか分からない) のでそこら中で型注釈してやる必要がある。(.) とか fmap で項を
-- 簡潔に書くはずが、項よりデカい型を書くハメになるという本末転倒っぷり。
productEg :: (String,Int)
productEg = f $> ()                       -- ("1",1) を返す
    where Product (f,g) $> x = (f x, g x) -- 結果を見せるためだけの補助関数
          f :: (Hask :× Hask) ((), ()) (String, Int)
          f = showFst . plus1 . c0 . toΔ
          toΔ :: (Hask :× Hask) (a, a) (Δ a)
          toΔ = Product (id,id)
          c0 :: Num a => (Hask :× Hask) (Δ ()) (Δ a)
          c0 = fmap $ const 0   -- Product (const 0, const 0) と一緒
          plus1 :: Num a => (Hask :× Hask) (Δ a) (Δ a)
          plus1 = fmap (+1)     -- Product ((+1), (+1)) と一緒
          showFst :: Show a => (Hask :× Hask) (Δ a) (String, a)
          showFst = Product (show, id)

newtype Uncurry f a = Uncurry { getUncurry :: f (Fst a) (Snd a) }

-- で、双関手は全て普通の関手として表現し直せるんだけど、
-- Bifunctor f hom1 hom2 hom3 => Functor (Uncurry f) (hom1 :× hom2) hom3
-- とはならず hom3 = Hask で固定される。理由は Uncurry を剥がしたり貼り直したり
-- する射の実装が hom3 に依存するから。hom3 と一緒に Uncurry / getUncurry に対
-- 応する hom3 の射を取るようにすれば多分一般化出来る (だるいからやってない)。
instance Bifunctor f hom1 hom2 Hask =>
    Functor (Uncurry f) (hom1 :× hom2) Hask where
      fmap :: forall a b. (hom1 :× hom2) a b -> Hask (Uncurry f a) (Uncurry f b)
      fmap (Product (f,g)) = liftUC $ bimap f g

liftUC :: (f (Fst p) (Snd p) -> g (Fst a) (Snd a)) -> Uncurry f p -> Uncurry g a
liftUC f (Uncurry x) = Uncurry $ f x


-- [まとめ]

-- Control.Category と HaskOp 使えば Contravariant は Functor に一元化できる。
-- Bifunctor も直積圏を定義すれば Functor に収容できるけど、newtype とか型注釈と
-- か乱れ飛んでウザい。Contravariant はともかく、Bifunctor の Functor 化は実用的
-- では無さそう。
