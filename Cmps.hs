-- import Data.Traversable

-- GHCi> maximum $ Cmps [Nothing, Just 2, Just 3]
-- 3
-- GHCi> length $ Cmps [[1,2], [], [3,4,5,6,7]]
-- 7
{-# LANGUAGE TypeOperators #-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq,Show)

instance (Functor f, Functor g) => Functor ((|.|) f g) where
    -- fmap func (Cmps cmps) = Cmps (fmap (\g -> (fmap func g)) cmps)
    fmap func (Cmps cmps) = Cmps $ (fmap . fmap) func cmps


instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    pure = Cmps . pure . pure
    (<*>) (Cmps fgf) (Cmps fga) = Cmps $ (<*>) <$> fgf <*> fga

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
    foldMap func (Cmps cmps) = foldMap (\f -> foldMap (\g -> func(g)) f) cmps
    -- foldMap func (Cmps cmps) = foldMap (foldMap func) cmps

-- [Just 1] >>= \x -> [x >>= \y -> Just (y + 1)]
instance (Monad f, Monad g, Traversable f, Traversable g) => Monad ((|.|) f g) where
    return = pure
    Cmps c >>= f = -- — step1, c is now f g a
        let fgfgb = fmap (fmap (getCmps . f)) c -- step2
            gffgb = sequence fgfgb -- step3
            gfgb = fmap (>>= id) gffgb -- step4
            fggb = sequence gfgb -- step5
            fgb = fmap (>>= id) fggb -- step6
            in Cmps fgb -- step7

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 cmps3 = fmap getCmps (getCmps cmps3)
-- unCmps3 = fmap getCmps . getCmps

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
-- unCmps4 cmps4 = fmap (\x -> fmap getCmps (getCmps x)) (getCmps cmps4)
unCmps4 cmps4 = fmap (fmap getCmps) (fmap getCmps (getCmps cmps4))
