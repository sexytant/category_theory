{-# LANGUAGE ExplicitForAll #-}

uncheck1 :: (forall b . (a -> b) -> b) -> a
uncheck1 t = t id

