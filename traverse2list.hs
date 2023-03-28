traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
-- traverse2list f = foldr ((<*>) . f) (pure [])
traverse2list func = foldr (\x -> \acc -> pure (:) <*> (func x) <*> acc) (pure [])
