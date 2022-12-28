safehead :: [] a -> Maybe a
safehead []     = Nothing
safehead (x:xs) = Just x

-- suggested tests



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

:{
safehead . concat . (map.map) length $ 
[["hello", "Bon jour"],["Guten Tag"], ["abc","def","gh"]]
:}
:{
fmap length . safehead . concat $
[["hello", "Bon jour"],["Guten Tag"], ["abc","def","gh"]]
:}

safehead . concat . (map.map) length $ [[]]

fmap length . safehead . concat $ [[]]
