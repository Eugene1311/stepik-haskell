data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
    fmap g (Tr x y z) = Tr (g x) (g y) (g z)

instance Applicative Triple where
    pure x = Tr x x x
    (Tr g1 g2 g3) <*> (Tr x y z) = Tr (g1 x) (g2 y) (g3 z)

instance Foldable Triple where
    foldr f ini (Tr x y z) = f x (f y (f z ini))
    foldl f ini (Tr x y z) = f (f (f ini x) y) z

-- traverse :: (Applicative f) => (a -> f b) -> Triple a -> f (Triple b)
instance Traversable Triple where
    traverse f (Tr x y z) = pure Tr <*> (f x) <*> (f y) <*> (f z)
