-- GHCi> maximum $ Cmps [Nothing, Just 2, Just 3]
-- 3
-- GHCi> length $ Cmps [[1,2], [], [3,4,5,6,7]]
-- 7
{-# LANGUAGE TypeOperators #-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

instance (Functor f, Functor g) => Functor ((|.|) f g) where
    fmap func (Cmps cmps) = Cmps (fmap (\g -> (fmap func g)) cmps)

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
    foldMap func (Cmps cmps) = foldMap (\f -> foldMap (\g -> func(g)) f) cmps
    -- foldMap func (Cmps cmps) = foldMap (foldMap func) cmps
