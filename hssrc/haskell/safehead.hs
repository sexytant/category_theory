safehead :: [] a -> Maybe a
safehead []     = Nothing
safehead (x:xs) = Just x

{-- compare the following with GHCi
:{
fmap length 
     (safehead ["hello", "Bon jour", "Guten Tag", "Buenos dias"])
:}
:{
safehead (map length
              ["hello", "Bon jour", "Guten Tag", "Buenos dias"])
:}

fmap length (safehead [])

safehead (map length [])
-}