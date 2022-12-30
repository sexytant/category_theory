alpha :: Maybe a -> Either () a
alpha Nothing  = Left ()
alpha (Just x) = Right x

beta :: Either () a -> Maybe a
beta (Left ()) = Nothing
beta (Right x) = Just x

{- suggested tests
beta . alpha $ Just 100
beta . alpha $ Nothing
alpha . beta $ Right 100
alpha . beta $ Left ()
-}
