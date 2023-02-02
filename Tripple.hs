data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
    foldr f ini (Tr x y z) = f x (f y (f z ini))
    foldl f ini (Tr x y z) = f (f (f ini x) y) z
