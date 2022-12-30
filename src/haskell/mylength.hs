import Data.Functor.Const

mylength :: [b] -> Const Int b
mylength []     = Const 0
mylength (x:xs) = Const (1 + getConst (mylength xs))

{- suggested test
:{
mylength . map (\xs -> xs ++ xs) $
  ["hello", "Bon jour", "Guten Tag", "Buenos dias"]
:}
:{
fmap (\xs -> xs ++ xs) . mylength $
  ["hello", "Bon jour", "Guten Tag", "Buenos dias"]
:}
-}

{- works fine with the following
mylength :: [b] -> Const Int b
mylength []     = 0
mylength (x:xs) = 1 + mylength xs
-}