data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
    fmap g (Tr x y z) = Tr (g x) (g y) (g z)

instance Applicative Triple where
    pure x = Tr x x x
    (Tr g1 g2 g3) <*> (Tr x y z) = Tr (g1 x) (g2 y) (g3 z)
